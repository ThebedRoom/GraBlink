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
    pmap: HashMap<Position, Index>
}

impl IOPair {
    fn new(input: String, output: String) -> IOPair {
        IOPair { 
            input: input, 
            output: output, 
            positions: vec![],
            pmap: HashMap::new()
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
#[derive(Clone, Eq)]
pub enum Program {
    ConstantStr(String),
    SubStr((Position,Index), (Position,Index)),
    Concat(Vec<Program>),
}

/// Evaluated indices for substrings do not affect equality
impl PartialEq for Program {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ConstantStr(l0), Self::ConstantStr(r0)) => l0 == r0,
            (Self::SubStr(l0, l1), Self::SubStr(r0, r1)) => l0.0 == r0.0 && l1.0 == r1.0,
            (Self::Concat(l0), Self::Concat(r0)) => l0 == r0,
            _ => false,
        }
    }
}

/// Evaluated indices for substrings shouldn't be hashed
impl std::hash::Hash for Program {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::ConstantStr(s) => { s.hash(state); }
            Self::SubStr((p1,_), (p2,_)) => {
                p1.hash(state);
                p2.hash(state);
            }
            Self::Concat(v) => {
                for p in v.iter() {
                    p.hash(state);
                }
            }
        }
    }
}

impl Program {
    /// Evaluate program on `input`
    fn evaluate(&self, input: &IOPair) -> Option<String> {
        match self {
            Self::ConstantStr(s) => Some(s.clone()),
            Self::SubStr(p1, p2) => {
                let start = match p1 {
                    (Position::ConstantPos(p),_) => Some(p),
                    (rp,_) => input.pmap.get(&rp)
                };
                let end = match p2 {
                    (Position::ConstantPos(p),_) => Some(p),
                    (rp,_) => input.pmap.get(&rp)
                };
                match (start, end) {
                    (Some(s), Some(e)) => match input.input.get(s.clone()..e.clone()) {
                        Some(s) => Some(String::from(s)),
                        None => None,
                    },
                    _ => None
                }
                
            },
            Self::Concat(v) => {
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
            // let e = self.evaluate(&pair);
            // match e {
            //     Some(s) => {println!("{}->{}",pair.input, s);}
            //     None => {}
            // }
            if self.evaluate(&pair) != Some(pair.output.clone()) {
                return false;
            }
        }
        true
    }

    /// Cost of a program (based on size and number of regexes)
    fn cost(&self) -> Cost {
        match self {
            Self::ConstantStr(_) => 1,
            Self::SubStr(p1, p2) => {
                let scost = match p1 {
                    (Position::ConstantPos(_),_) => 1,
                    (Position::RegexPos(_, _, _),_) => 2,
                };
                let ecost = match p2 {
                    (Position::ConstantPos(_),_) => 1,
                    (Position::RegexPos(_, _, _),_) => 2,
                };
                scost + ecost + 1
            },
            Self::Concat(v) => {
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
            Self::ConstantStr(s) => format!("ConstantStr({})", s),
            Self::SubStr(p1, p2) => format!("Substr({},{})[{},{}]", p1.0, p2.0,p1.1,p2.1),
            Self::Concat(ss) => {
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
}

impl Edge {
    fn new() -> Edge {
        Edge { programs: HashSet::new() }
    }

    fn add_substr(&mut self, p1: (Position, Index), p2: (Position, Index)) {
        self.programs.insert(Program::SubStr(p1, p2));
    }

    fn add_str(&mut self, s: String) {
        self.programs.insert(Program::ConstantStr(s));
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
        let mut edge = Edge::new();
        for p in iter {
            match p {
                Program::ConstantStr(_) => { edge.programs.insert(p); },
                Program::SubStr(p1, p2) => {
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
        let mut cmap = HashMap::new();

        // constant strings
        for i in 0..n+1 {
            for j in i+1..n+1 {
                let mut edge = Edge::new();
                let s = io.output.get(i..j).unwrap().to_string();
                edge.add_str(s.clone());
                match dag.add_edge(NodeIndex::new(i), NodeIndex::new(j), edge) {
                    Ok(i) => {
                        cmap.insert(s, i);
                    }
                    Err(_) => {}//shouldn't ever happen
                }
            }
        }
        let mut const_pos: Vec<(Position, Index)> = (0..io.input.len()+1).map(|i| (Position::ConstantPos(i),i)).collect();
        let mut valid_positions = io.positions.clone();
        valid_positions.append(&mut const_pos);
        valid_positions.sort_by(|x,y| x.1.cmp(&y.1));

        for i in 0..valid_positions.len() {
            for j in i+1..valid_positions.len() {
                if valid_positions[i].1 == valid_positions[j].1 { continue; }
                match Program::SubStr(valid_positions[i].clone(), valid_positions[j].clone()).evaluate(&io) {
                    Some(out) => {
                        match cmap.get(&out) {
                            Some(edge) => {
                                dag.edge_weight_mut(*edge).unwrap()
                                    .add_substr(valid_positions[i].clone(), valid_positions[j].clone());
                            },
                            None => {},
                        }
                    },
                    None => {},
                }
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
            is[i].pmap.insert(p.clone(), v[i]);
        }
    }
    // Sort positions
    for i in is.iter_mut() {
        i.positions.sort_by(|x,y| x.1.cmp(&y.1));
    }

    let mut odg = InputDataGraph::<Edge>::new(&is[0]);
    odg.to_dot("test.dot", true);
    for i in is.iter().skip(1) {
        let t = InputDataGraph::<Edge>::new(i);
        t.to_dot("test2.dot", true);
        odg = odg.intersection(t, true);
    }
    odg.to_dot("test3.dot", true);

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