use std::collections::{HashSet, BTreeSet, HashMap, VecDeque};
use std::fmt::Display;
use std::thread;
use regex::Match;
use itertools::Itertools;

use crate::inputdatagraph::{InputDataGraph, Intersectable, PMatch, NodeID, TOKENS};
use daggy::{Dag, NodeIndex, EdgeIndex, Walker};

type Cost = usize;
type Index = usize;

/// Represents an input/output pair
pub struct IOPair {
    pub input: String,
    pub output: String,
    /// All valid positions for this I/O pair
    pub positions: Vec<Position>,
    /// Map of Position to the index in `input` it evaluates to
    pmap: HashMap<Position, Index>,
    /// numerical substrings in `input`
    nums: HashMap<i64, Vec<Number>>,
}

impl IOPair {
    pub fn new(input: String, output: String, with_nums: bool) -> Self {
        let mut nums: HashMap<i64, Vec<Number>> = HashMap::new();
        if with_nums {
            for i in 0..input.len() {
                for j in i+1..input.len()+1 {
                    match &input[i..j].parse() {
                        Ok(n) => {
                            if !nums.contains_key(n) {
                                nums.insert(*n, vec![Number::ConstantNum(*n)]);
                            }
                        },
                        Err(_) => {},
                    }
                }
            }
        }
        IOPair { 
            input: input, 
            output: output, 
            positions: vec![],
            pmap: HashMap::new(),
            nums: nums
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

#[allow(dead_code)]
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Number {
    ConstantNum(i64),
    StrToNum(Box<Program>),
    Add(Box<Number>, Box<Number>),
    Times(Box<Number>, Box<Number>),
}

impl Number {
    pub fn evaluate(&self, input: &IOPair) -> Option<i64> {
        match self {
            Self::ConstantNum(n) => Some(n.clone()),
            Self::Add(n1, n2) => {
                Some(n1.evaluate(input)? + n2.evaluate(input)?)
            },
            Self::Times(n1, n2) => {
                Some(n1.evaluate(input)? * n2.evaluate(input)?)
            },
            Self::StrToNum(s) => {
                let res = s.as_ref().evaluate(input);
                res?.parse().ok()
            }
        }
    }

    pub fn cost(&self) -> Cost {
        match self {
            Self::ConstantNum(_) => 1,
            Self::StrToNum(p) => 1 + p.cost(),
            Self::Add(n1, n2) => 1 + n1.cost() + n2.cost(),
            Self::Times(n1, n2) => 1 + n1.cost() + n2.cost(),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::ConstantNum(n) => n.to_string(),
            Self::Add(n1, n2) => format!("{} + {}", n1.to_string(), n2.to_string()),
            Self::Times(n1, n2) => format!("{} * {}", n1.to_string(), n2.to_string()),
            Self::StrToNum(n) => format!("StrToNum({})", n)
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
                let start = input.pmap.get(&p1)?;
                let end = input.pmap.get(&p2)?;
                Some(String::from(input.input.get(start.clone()..end.clone())?))
            },
            Self::Concat(v) => {
                let mut out = String::new();
                for s in v {
                    out.push_str(s.evaluate(input)?.as_str());
                }
                Some(out)
            },
            Self::NumToStr(number) => {
                Some(number.evaluate(input)?.to_string())
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
            Self::NumToStr(n) => 1 + n.cost(),
        }
    }
}

fn enumerate_nums<T>(dag: &mut Dag<T, Edge>, nums: &mut VecDeque<(i64, Vec<Number>)>, num_edges: &HashMap<i64, EdgeIndex>) {
    if nums.len() > 1 {
        let (n1, n1vec) = nums.pop_front().unwrap();
        let (n2, n2vec) = nums.pop_front().unwrap();
        let sum = n1 + n2;
        match num_edges.get(&sum) {
            Some(i) => {
                for op1 in n1vec.iter() {
                    for op2 in n2vec.iter() {
                        let p = Number::Add(Box::new(op1.to_owned()), Box::new(op2.to_owned()));
                        dag.edge_weight_mut(*i).unwrap().add_num(p.to_owned());
                        nums.push_front((sum, vec![p]));
                        enumerate_nums(dag, nums, num_edges)
                    }
                }
            },
            None => {},
        }
        let product = n1 * n2;
        match num_edges.get(&product) {
            Some(i) => {
                for op1 in n1vec.iter() {
                    for op2 in n2vec.iter() {
                        let p = Number::Times(Box::new(op1.to_owned()), Box::new(op2.to_owned()));
                        dag.edge_weight_mut(*i).unwrap().add_num(p.to_owned());
                        nums.push_front((sum, vec![p]));
                        enumerate_nums(dag, nums, num_edges)
                    }
                }
            },
            None => {},
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
        match s.parse::<i64>() {
            Ok(n) => {
                self.programs.insert(Program::NumToStr(Number::ConstantNum(n)));
            },
            Err(_) => {
                self.programs.insert(Program::ConstantStr(s));
            },
        }
    }

    fn add_num(&mut self, n: Number) {
        self.programs.insert(Program::NumToStr(n));
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
pub fn regex_match(token: &String, k: i32, input: &String) -> Option<(Index,Index)> {
    let matches: Vec<Match> = TOKENS.get(token.as_str())?.find_iter(input).collect();
    let idx = if k >= 0 {
        k - 1
    } else {
        (matches.len() as i32) + k
    } as Index;
    let mtch = matches.iter().nth(idx)?;
    Some((mtch.start(), mtch.end()))
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
        for i in 0..io.input.len()+1 {
            io.pmap.insert(Position::ConstantPos(i), i);
        }
        let mut cmap = HashMap::new();
        let mut num_edges = HashMap::new();

        // constant strings
        for i in 0..n+1 {
            for j in i+1..n+1 {
                let mut edge = Edge::new();
                let s = io.output.get(i..j).unwrap().to_string();
                edge.add_str(s.clone());
                match dag.add_edge(NodeIndex::new(i), NodeIndex::new(j), edge) {
                    Ok(i) => {
                        if io.nums.len() > 0 {
                            let _ = s.parse::<i64>().and_then(|x| { num_edges.insert(x, i); Ok(())});
                        }
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
                if io.pmap.get(&valid_positions[i]) == io.pmap.get(&valid_positions[j]) { continue; }
                match Program::SubStr(valid_positions[i].clone(), valid_positions[j].clone()).evaluate(&io) {
                    Some(out) => {
                        match cmap.get(&out) {
                            Some(edge) => {
                                dag.edge_weight_mut(*edge).unwrap()
                                    .add_substr(
                                        valid_positions[i].clone(), 
                                        valid_positions[j].clone()
                            );
                            }
                            None => {}
                        }
                        if io.nums.len() > 0 {
                            let _ = out.parse::<i64>().and_then(|n| {
                                io.nums.get_mut(&n).unwrap().push(
                                    Number::StrToNum(Box::new(Program::SubStr(
                                        valid_positions[i].clone(),
                                        valid_positions[j].clone()
                                    )
                                )));
                                Ok(())
                            });
                        }
                    },
                    None => {},
                }
            }
        }

        // numeric expressions
        for exps in io.nums.iter().permutations(io.nums.len()).unique().into_iter() {
            enumerate_nums(&mut dag, &mut exps.iter().map(
                |&x| (x.0.to_owned(),x.1.to_owned())
            ).collect(), &num_edges);
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

pub fn gen_program(input: &'static Vec<String>, ncols: usize, output_odg: &Option<String>, with_nums: bool) -> Option<Program> {
    let mut is: Vec<IOPair> = input.chunks(2)
        .map(|x| IOPair::new(x[0].clone(),x[1].clone(),with_nums))
        .collect();

    let mut threads = vec![];
    if input.len() % ncols != 0 {
        panic!("Rows are not all the same length!");
    }
    for i in 0..ncols - 1 {
        let col = input.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(move || {
            InputDataGraph::gen_graph_column(col, true, true)
        }));
    }
    let idgs: Vec<InputDataGraph<HashSet<PMatch>>> = threads
        .into_iter()
        .map(|x| x.join().unwrap())
        .collect();
    match output_odg {
        Some(s) => {
            for n in 0..idgs.len() {
                let mut fname = s.to_owned();
                fname.push_str(n.to_string().as_str());
                fname.push_str(".dot");
                idgs[n].to_dot(fname.as_str(), true);
            }
        }
        None => {}
    }

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
                    temp.get_mut(&Position::RegexPos(p.tau.clone(), p.k, Dir::Start)).unwrap().push(start);
                    temp.get_mut(&Position::RegexPos(p.tau.clone(), p.k, Dir::End)).unwrap().push(end);
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
    if output_odg != &None {
        let mut fname = String::from(&is[0].output);
        fname.push_str("_odg.dot");
        odg.to_dot(&fname, true);
    }
    for i in is.iter_mut().skip(1) {
        let temp = InputDataGraph::<Edge>::new(i);
        if output_odg != &None {
            let mut fname = String::from(&i.output);
            fname.push_str("_odg.dot");
            temp.to_dot(&fname, true);
        }
        odg = odg.intersection(temp, true);
        
    }
    if output_odg != &None {
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
        println!("Space size  : {}", choices.len());

        for p in choices {
            let t = Program::Concat(p.1);
            if t.verify(&is) {
                println!("Program Cost: {}", p.0);
                return Some(t);
            }
        }
        None
    }
}