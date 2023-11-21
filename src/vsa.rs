use std::collections::hash_map::RandomState;
use std::collections::{HashSet, BTreeSet, HashMap};
use std::fmt::Display;
use std::thread;
use regex::Match;

use crate::inputdatagraph::{InputDataGraph, Intersectable, PMatch, NodeID, TOKENS};
use daggy::{Dag, NodeIndex, Walker};

type Cost = usize;
type Index = usize;

/// Represents an input/output pair
struct IOPair {
    input: String,
    output: String,
    positions: Vec<(Position, Index)>,
}

impl IOPair {
    fn new(input: String, output: String) -> IOPair {
        IOPair { 
            input: input, 
            output: output, 
            positions: vec![]
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Dir {
    Start, End
}

/// Represents a position in a string
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Position {
    ConstantPos(Index),
    RegexPos(String, i32, Dir),
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Position::ConstantPos(n) => n.to_string(),
            Position::RegexPos(t, k, d) => format!(
                "({}, {}, {})", t, k, if *d == Dir::Start { "START" } else { "END" })
        })
    }
}

/// The DSL
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Program {
    ConstantStr(String),
    SubStr((Position,Index), (Position,Index)),
    Concat(Vec<Program>),
}

impl Program {
    /// Evaluate program on `input`
    fn evaluate(&self, input: &String) -> Option<String> {
        match self {
            Program::ConstantStr(s) => Some(s.clone()),
            Program::SubStr(p1, p2) => {
                let start = match p1 {
                    (Position::ConstantPos(p),_) => Some(p.clone()),
                    (Position::RegexPos(_,_,_),i) => Some(*i)
                };
                let end = match p2 {
                    (Position::ConstantPos(p),_) => Some(p.clone()),
                    (Position::RegexPos(_,_,_),i) => Some(*i)
                };
                match (start, end) {
                    (Some(s), Some(e)) => match input.get(s.clone()..e.clone()) {
                        Some(s) => Some(String::from(s)),
                        None => None,
                    },
                    _ => None
                }
                
            },
            Program::Concat(v) => {
                let mut out = String::new();
                for s in v {
                    match s.evaluate(input) {
                        Some(t) => { out.push_str(t.as_str()); }
                        None => { return None; }
                    }
                }
                Some(out)
            },
        }
    }

    // Verify that program is correct for all I/O pairs
    fn verify(&self, io: &Vec<IOPair>) -> bool {
        for pair in io.iter() {
            // let e = self.evaluate(&pair.input);
            // match e {
            //     Some(s) => {println!("{}->{}",pair.input, s);}
            //     None => {}
            // }
            if self.evaluate(&pair.input) != Some(pair.output.clone()) {
                return false;
            }
        }
        true
    }

    fn cost(&self) -> Cost {
        match self {
            Program::ConstantStr(_) => 1,
            Program::SubStr(p1, p2) => {
                let scost = match p1 {
                    (Position::ConstantPos(_),_) => 1,
                    (Position::RegexPos(_, k, _),_) => if *k < 1 {2} else {1},
                };
                let ecost = match p1 {
                    (Position::ConstantPos(_),_) => 1,
                    (Position::RegexPos(_, k, _),_) => if *k < 1 {2} else {1},
                };
                scost + ecost
            },
            Program::Concat(v) => {
                let mut cost = 0;
                for p in v.iter() {
                    cost += p.cost();
                }
                cost
            },
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prog = match self {
            Program::ConstantStr(s) => format!("ConstantStr({})", s),
            Program::SubStr(p1, p2) => format!("Substr({},{})[{},{}]", p1.0, p2.0,p1.1,p2.1),
            Program::Concat(ss) => {
                let mut l = String::new();
                for s in ss.iter() {
                    l.push_str(s.to_string().as_str());
                    l.push_str(", ");
                }
                format!("Concat({})", l)
            }
        };
        write!(f, "{}", prog)
    }
}

/// An edge in the IDG, containing complete programs that represent that edge
#[derive(Clone)]
struct Edge {
    programs: HashSet<Program>,
    pos1: Vec<(Position, Index)>,
    pos2: Vec<(Position, Index)>,
    input: String,
    conststr: String
}

impl Edge {
    fn new(input: String) -> Edge {
        Edge { 
            programs: HashSet::new(),
            pos1: vec![], 
            pos2: vec![],
            input: input,
            conststr: String::new() 
        }
    }

    fn add_substr(&mut self, p1: Index, p2: Index) -> Result<(),String> {
        if p1 >= p2 {
            Err(format!("Invalid positions: [{}, {}]", p1, p2)) 
        } else {
            self.pos1.push((Position::ConstantPos(p1.clone()), p1));
            self.pos2.push((Position::ConstantPos(p2.clone()), p2));
            self.programs.insert(Program::SubStr(
                (Position::ConstantPos(p1),p1), 
                (Position::ConstantPos(p2),p2)
            ));
            Ok(())
        }
    }

    fn add_str(&mut self, s: String) {
        self.programs.insert(Program::ConstantStr(s));
    }

    fn add_tk_pos(&mut self, p: &Position, i: Index, direction: Dir) {
        match p {
            Position::ConstantPos(_) => {},
            _ => {
                let mut modified = false;
                if direction == Dir::Start {
                    for p2 in self.pos2.iter() {
                        if p2.1 <= i { continue; }
                        let prog = Program::SubStr((p.clone(),i),p2.clone());
                        let s = prog.evaluate(&self.input);
                        println!("{}",prog);
                        match s {
                            Some(ss) => {println!("{},{}",ss,self.conststr)}
                            None=>{}
                        }
                        if prog.evaluate(&self.input) == Some(self.conststr.clone()) {
                            self.programs.insert(prog);
                            modified = true;
                        }
                    }
                    if modified {
                        self.pos1.push((p.clone(),i));
                    }
                } else {
                    for p1 in self.pos1.iter() {
                        if p1.1 >= i { continue; }
                        let prog = Program::SubStr(p1.clone(), (p.clone(),i));
                        let s = prog.evaluate(&self.input);
                        println!("{}",prog);
                        match s {
                            Some(ss) => {println!("{},{}",ss,self.conststr)}
                            None=>{}
                        }
                        if prog.evaluate(&self.input) == Some(self.conststr.clone()) {
                            self.programs.insert(prog);
                            modified = true;
                        }
                    }
                    if modified {
                        self.pos2.push((p.clone(),i));
                    }
                }
            }
        }
    }
}

impl<'a> Intersectable<'a, Edge> for Edge {
    type Item = Program;

    fn intersection(&'a self, other: &'a Edge) -> std::collections::hash_set::Intersection<'a, Self::Item, std::collections::hash_map::RandomState> {
        self.programs.intersection(&other.programs)
    }

    fn is_empty(&self) -> bool {
        self.programs.is_empty()
    }

    fn iter(&self) -> std::collections::hash_set::Iter<'_, Self::Item> {
        self.programs.iter()
    }
}

impl FromIterator<Program> for Edge {
    fn from_iter<T: IntoIterator<Item = Program>>(iter: T) -> Self {
        let mut edge = Edge::new(String::new());
        for p in iter {
            match p {
                Program::ConstantStr(_) => { edge.programs.insert(p); },
                Program::SubStr(p1, p2) => {
                    edge.pos1.push((p1.0.clone(),p1.1));
                    edge.pos2.push((p2.0.clone(),p2.1));
                    edge.programs.insert(Program::SubStr(p1, p2));
                },
                Program::Concat(_) => {
                    panic!("Tried to put a Concat into an edge");
                }
            }
        }
        edge
    }
}

/// Start and End index of the `k`th match of `token` in `input`
fn regex_match(token: &String, k: i32, input: &String) -> Option<(Index,Index)> {
    let matches: Vec<Match> = TOKENS.get(token.as_str()).unwrap().find_iter(input).collect();
    let idx = if k >= 0 {
        k - 1
    } else {
        (matches.len() as i32) + k
    } as Index;
    let mtch = matches.iter().nth(idx);
    match mtch {
        Some(m) => Some((m.start(), m.end())),
        None => None
    }
}

impl InputDataGraph<Edge> {
    fn new(io: &IOPair) -> Self {
        // init dag
        let n = io.output.len();
        let mut dag: Dag<BTreeSet<NodeID>, Edge> =
            Dag::with_capacity(n + 1, (n * (n + 1)) / 2);
        for i in 0..n as u32 + 1 {
            dag.add_node(BTreeSet::from([NodeID::new(io.output.clone(), i, 0)]));
        }

        // constant strings & positions
        for i in 0..n+1 {
            for j in i+1..n+1 {
                let mut edge = Edge::new(io.input.clone());
                let s = io.output.get(i..j).unwrap().to_string();
                edge.add_str(s.clone());
                edge.conststr = s;
                match edge.add_substr(i, j) {
                    Ok(()) => {}
                    Err(e) => { println!("{}",e); }
                }
                let _ = dag.add_edge(
                    NodeIndex::new(i), 
                    NodeIndex::new(j), 
                    edge
                );
            }
        }

        for edge in dag.edge_weights_mut() {
            for (p,i) in io.positions.iter() {
                edge.add_tk_pos(p, *i, Dir::Start);
            }
            for (p,i) in io.positions.iter() {
                edge.add_tk_pos(p, *i, Dir::End);
            }
        }

        InputDataGraph { dag: dag }
    }

    fn extract(&self, i: NodeIndex) -> Vec<(Cost, Vec<Program>)> {
        let mut out = vec![];
        for n in self.dag.children(i).iter(&self.dag) {
            let edge_programs: Vec<(Cost, Program)> = self.dag.edge_weight(n.0).unwrap()
                .programs.iter()
                .map(|p| (p.cost(),p.clone()))
                .collect();
            for (es, ep) in edge_programs {
                let ps = self.extract(n.1);
                if ps.len() > 0 {
                    let mut cprog: Vec<(Cost, Vec<Program>)> = ps.iter()
                        .map(|(s,prog)| {
                            let mut pp: Vec<Program> = prog.clone();
                            pp.insert(0, ep.clone());
                            (s + es, pp)
                        })
                        .collect();
                    out.append(&mut cprog);
                } else {
                    out.push((es, vec![ep]));
                }
            }
        }
        out
    }
}

pub fn gen_program(input: &'static Vec<String>, ncols: usize) -> Option<Program> {
    let mut is: Vec<IOPair> = input.chunks(2)
            .map(|x| IOPair::new(x[0].clone(),x[1].clone()))
            .collect();

    let mut threads = vec![];
    for i in 0..ncols-1 {
        let col: Vec<String> = input.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(|| InputDataGraph::gen_graph_column(col, true)));
    }
    let idgs: Vec<InputDataGraph<HashSet<PMatch>>> = threads
        .into_iter()
        .map(|x| x.join().unwrap())
        .collect();

    // pmatches
    let pmatches: HashSet<PMatch> = idgs.iter().map(
        |idg| {
            idg.dag.raw_edges().iter().map(
                |x| {
                    x.weight.clone()
                }
            )
        }
    ).flatten()
    .flatten()
    .filter(|x| !x.constantstr && x.tau != "StartT" && x.tau != "EndT")
    .collect();

    // pre-filtering of useless or bad positions
    let mut temp: HashMap<Position,Vec<Index>> = pmatches.iter().map(|pm| {
        (Position::RegexPos(pm.tau.clone(), pm.k, Dir::Start), vec![])
    }).collect();
    temp.extend(pmatches.iter().map(|pm| {
        (Position::RegexPos(pm.tau.clone(), pm.k, Dir::End), vec![])
    }));
    for p in pmatches.iter() {
        for i in is.iter() {
            let rm = regex_match(&p.tau, p.k, &i.input);
            match rm {
                Some((start,end)) => {
                    match temp.get_mut(&Position::RegexPos(p.tau.clone(), p.k, Dir::Start)) {
                        Some(v) => {
                            v.push(start);
                        }
                        None => {}
                    }
                    match temp.get_mut(&Position::RegexPos(p.tau.clone(), p.k, Dir::End)) {
                        Some(v) => {
                            v.push(end);
                        }
                        None => {}
                    }
                }
                None => {
                    let _ = temp.remove(&Position::RegexPos(p.tau.clone(), p.k, Dir::Start));
                    let _ = temp.remove(&Position::RegexPos(p.tau.clone(), p.k, Dir::End));
                }
            }
        }
    }
    // filter out ones that are equivalent to ConstantPos
    temp.retain(|_,v| {
        HashSet::<&Index, RandomState>::from_iter(v.iter()).len() > 1
    });
    // Avoid recomputing regexpos in the future
    for (p,v) in temp.iter() {
        for i in 0..is.len() {
            is[i].positions.push((p.clone(), v[i]));
        }
    }
    // Sort positions
    for i in is.iter_mut() {
        i.positions.sort_by(|x,y| x.1.cmp(&y.1));
    }

    let mut odg = InputDataGraph::<Edge>::new(&is[0]);
    odg.to_dot("test.dot", true);
    for i in is.iter().skip(1) {
        odg = odg.intersection(InputDataGraph::<Edge>::new(i), true);
    }
    odg.to_dot("test2.dot", true);
    
    if odg.dag.edge_count() == 0 {
        None
    } else {
        let mut index = 0;
        for i in 0..odg.dag.raw_nodes().iter().len() {
            if odg.dag.node_weight(NodeIndex::new(i)).unwrap().first().unwrap().is_first() {
                index = i;
                break;
            }
        }
        
        let mut choices = odg.extract(NodeIndex::new(index));
        choices.sort_by(|x,y| x.0.cmp(&y.0));

        for p in choices {
            let t = Program::Concat(p.1);
            println!("{}",t);
            if t.verify(&is) {
                return Some(t);
            }
        }
        None
    }
}