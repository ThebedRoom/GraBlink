use std::collections::{HashSet, BTreeSet, HashMap};
use std::fmt::Display;
use std::thread;
use regex::Match;

use crate::inputdatagraph::{InputDataGraph, Intersectable, PMatch, NodeID, TOKENS};
use daggy::{Dag, NodeIndex, Walker};

type Cost = usize;
type Index = usize;

/// Represents an input/output pair
pub struct IOPair {
    input: String,
    output: String,
    /// All valid positions for this I/O pair
    positions: Vec<Position>,
    /// Map of Position to the index in `input` it evaluates to
    pmap: HashMap<Position, Index>
}

impl IOPair {
    pub fn new(input: String, output: String) -> IOPair {
        IOPair { 
            input: input, 
            output: output, 
            positions: vec![],
            pmap: HashMap::new()
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Dir {
    Start, End
}

/// Represents a position in a string
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Number {
    ConstantNum(usize),
    SubNum(Position, Position),
    Add(Box<Number>, Box<Number>),
}

impl Number {
    fn evaluate(&self, input: &IOPair) -> Option<usize> {
        match self {
            Number::ConstantNum(n) => Some(n.clone()),
            Number::SubNum(p1, p2) => {
                match Program::SubStr(p1.clone(), p2.clone()).evaluate(input) {
                    Some(s) => {
                        match s.parse() {
                            Ok(n) => Some(n),
                            _ => None
                        }
                    },
                    None => None,
                }
            },
            Number::Add(n1, n2) => {
                match (n1.evaluate(input), n2.evaluate(input)) {
                    (Some(o1), Some(o2)) => Some(o1 + o2),
                    _ => None
                }
            },
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Number::ConstantNum(n) => n.to_string(),
            Number::SubNum(p1, p2) => format!("SubNum({},{})",p1,p2),
            Number::Add(n1, n2) => format!("{} + {}", n1.to_string(), n2.to_string()),
        })
    }
}

/// The DSL
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Program {
    ConstantStr(String),
    SubStr(Position, Position),
    Concat(Vec<Program>),
    NumToStr(Number),
}

impl Program {
    /// Evaluate program on `input`
    fn evaluate(&self, input: &IOPair) -> Option<String> {
        match self {
            Self::ConstantStr(s) => Some(s.clone()),
            Self::SubStr(p1, p2) => {
                let start = match p1 {
                    Position::ConstantPos(p) => Some(p),
                    rp => input.pmap.get(&rp)
                };
                let end = match p2 {
                    Position::ConstantPos(p) => Some(p),
                    rp => input.pmap.get(&rp)
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
            Self::NumToStr(number) => {
                match number.evaluate(input) {
                    Some(n) => Some(n.to_string()),
                    None => None
                }
            }
        }
    }

    /// Verify that program is correct for all I/O pairs
    fn verify(&self, io: &Vec<IOPair>) -> bool {
        for pair in io.iter() {
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
                    Position::ConstantPos(_) => 1,
                    Position::RegexPos(_, _, _) => 2,
                };
                let ecost = match p2 {
                    Position::ConstantPos(_) => 1,
                    Position::RegexPos(_, _, _) => 2,
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
            Self::NumToStr(_) => 1,
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prog = match self {
            Self::ConstantStr(s) => format!("ConstantStr({})", s),
            Self::SubStr(p1, p2) => format!("Substr({},{})", p1, p2),
            Self::Concat(ss) => {
                let mut l = String::new();
                for s in ss.iter() {
                    l.push_str(s.to_string().as_str());
                    l.push_str(", ");
                }
                format!("Concat({})", l)
            },
            Self::NumToStr(n) => format!("NumToStr({})", n.to_string()),
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

    fn add_substr(&mut self, p1: Position, p2: Position) {
        self.programs.insert(Program::SubStr(p1, p2));
    }

    fn add_str(&mut self, s: String) {
        match s.parse::<usize>() {
            Ok(n) => {
                self.programs.insert(Program::NumToStr(Number::ConstantNum(n)));
            },
            Err(_) => {
                self.programs.insert(Program::ConstantStr(s));
            },
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
        let mut edge = Edge::new();
        for p in iter {
            match p {
                Program::Concat(_) => {
                    panic!("Tried to put a Concat into an edge");
                },
                _ => { 
                    edge.programs.insert(p); 
                },
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
    fn new(io: &mut IOPair) -> Self {
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
            io.pmap.insert(Position::ConstantPos(i), i);
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
        let mut valid_positions: Vec<Position> = io.pmap.keys().cloned().collect();
        valid_positions.sort_by(|x,y| io.pmap.get(x).unwrap().cmp(&io.pmap.get(y).unwrap()));

        for i in 0..valid_positions.len() {
            for j in i+1..valid_positions.len() {
                if valid_positions[i] == valid_positions[j] { continue; }
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

pub fn gen_program(input: &'static Vec<String>, ncols: usize, output_odg: bool) -> Option<Program> {
    let mut is: Vec<IOPair> = input.chunks(2)
        .map(|x| IOPair::new(x[0].clone(),x[1].clone()))
        .collect();

    let mut threads = vec![];
    for i in 0..ncols-1 {
        let col: Vec<String> = input.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(|| InputDataGraph::gen_graph_column(col, true, false)));
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
    .filter(|x| x.tau != "StartT" && x.tau != "EndT")
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
        HashSet::<&Index, std::collections::hash_map::RandomState>::from_iter(v.iter()).len() > 1
    });
    // Avoid recomputing regexpos in the future
    for (p,v) in temp.iter() {
        for i in 0..is.len() {
            is[i].positions.push(p.clone());
            is[i].pmap.insert(p.clone(), v[i]);
        }
    }
    // Sort positions
    for i in is.iter_mut() {
        i.positions.sort_by(|x,y| i.pmap.get(x).unwrap().cmp(&i.pmap.get(y).unwrap()));
    }

    let mut odg = InputDataGraph::<Edge>::new(&mut is[0]);
    if output_odg {
        let mut fname = String::from(&is[0].output);
        fname.push_str("_odg.dot");
        odg.to_dot(&fname, true);
    }
    for i in is.iter_mut().skip(1) {
        let temp = InputDataGraph::<Edge>::new(i);
        if output_odg {
            let mut fname = String::from(&i.output);
            fname.push_str("_odg.dot");
            temp.to_dot(&fname, true);
        }
        odg = odg.intersection(temp, true);
        
    }
    if output_odg {
        odg.to_dot("odg.dot", true);
    }

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
        println!("Space size: {}", choices.len());

        for p in choices {
            let t = Program::Concat(p.1);
            if t.verify(&is) {
                return Some(t);
            }
        }
        None
    }
}