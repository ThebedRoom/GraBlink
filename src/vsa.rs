use std::collections::{HashSet,BTreeSet};
use std::fmt::Display;
use std::thread;
use daggy::petgraph::visit::NodeRef;
use regex::Match;

use crate::inputdatagraph::{InputDataGraph,Intersectable,PMatch,NodeID,TOKENS};
use daggy::{Dag,NodeIndex,Walker};

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Position {
    ConstantPos(usize),
    RegexPos(String, i32, bool),
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Position::ConstantPos(n) => n.to_string(),
            Position::RegexPos(t, k, s) => format!(
                "({}, {}, {})", t, k, if *s { "START" } else { "END" })
        })
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Program {
    ConstantStr(String),
    SubStr(Position, Position),
    Concat(Vec<Program>),
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prog = match self {
            Program::ConstantStr(s) => format!("ConstantStr({})", s),
            Program::SubStr(p1, p2) => format!("Substr({},{})", p1, p2),
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

#[derive(Clone)]
struct Edge {
    programs: HashSet<Program>,
    pos1: HashSet<Position>,
    pos2: HashSet<Position>
}

impl Edge {
    fn new() -> Edge {
        Edge { 
            programs: HashSet::new(),
            pos1: HashSet::new(), 
            pos2: HashSet::new()
        }
    }

    fn add_substr(&mut self, p1: usize, p2: usize) -> Result<(),String> {
        if p1 >= p2 {
            Err(format!("Invalid positions: [{}, {}]", p1, p2)) 
        } else {
            self.pos1.insert(Position::ConstantPos(p1.clone()));
            self.pos2.insert(Position::ConstantPos(p2.clone()));
            self.programs.insert(Program::SubStr(
                Position::ConstantPos(p1), 
                Position::ConstantPos(p2)
            ));
            Ok(())
        }
    }

    fn add_str(&mut self, s: String) {
        self.programs.insert(Program::ConstantStr(s));
    }

    fn add_tk_pos(&mut self, p: Position, start: bool) {
        match p {
            Position::ConstantPos(_) => {},
            _ => {
                if start {
                    for p2 in self.pos2.iter() {
                        self.programs.insert(Program::SubStr(p.clone(), p2.clone()));
                    }
                    self.pos1.insert(p);
                } else {
                    for p1 in self.pos1.iter() {
                        self.programs.insert(Program::SubStr(p1.clone(), p.clone()));
                    }
                    self.pos2.insert(p);
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
        let mut edge = Edge::new();
        for p in iter {
            match p {
                Program::ConstantStr(_) => { edge.programs.insert(p); },
                Program::SubStr(p1, p2) => {
                    edge.pos1.insert(p1.clone());
                    edge.pos2.insert(p2.clone());
                    edge.programs.insert(Program::SubStr(p1, p2));
                },
                Program::Concat(ps) => {
                    panic!("Tried to put a Concat into an edge");
                }
            }
        }
        edge
    }
}

fn regex_match(token: &String, k: i32, input: &String) -> Option<(usize,usize)> {
    let matches: Vec<Match> = TOKENS.get(token.as_str()).unwrap().find_iter(input).collect();
    let idx = if k >= 0 {
        k - 1
    } else {
        (matches.len() as i32) + k
    } as usize;
    let mtch = matches.iter().nth(idx);
    match mtch {
        Some(m) => Some((m.start(), m.end())),
        None => None
    }
}

impl InputDataGraph<Edge> {
    fn new(output: &String, idgs: &Vec<InputDataGraph<HashSet<PMatch>>>) -> Self {
        // init dag
        let n = output.len();
        let mut dag: Dag<BTreeSet<NodeID>, Edge> =
            Dag::with_capacity(n + 1, (n * (n + 1)) / 2);
        for i in 0..n as u32 + 1 {
            dag.add_node(BTreeSet::from([NodeID::new(output.clone(), i, 0)]));
        }

        // constant strings & positions
        for i in 0..n+1 {
            for j in 0..n+1 {
                if i >= j { continue; }
                let mut edge = Edge::new();
                edge.add_str(output.get(i..j).unwrap().to_string());
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
        let mut add: Vec<(NodeIndex, NodeIndex, bool, Position)> = vec![];
        for edge in dag.raw_edges() {
            for i in pmatches.iter() {
                let pos = regex_match(&i.tau, i.k, output);
                match pos {
                    Some((start,end)) => {
                        if edge.source().index() == start {
                            add.push((
                                edge.source(), edge.target(), true,
                                Position::RegexPos(i.tau.clone(), i.k, true)
                            ));
                        }
                        if edge.source().index() == end {
                            add.push((
                                edge.source(), edge.target(), true,
                                Position::RegexPos(i.tau.clone(), i.k, false)
                            ));
                        }
                        if edge.target().index() == start {
                            add.push((
                                edge.source(), edge.target(), false,
                                Position::RegexPos(i.tau.clone(), i.k, true)
                            ));
                        }
                        if edge.target().index() == end {
                            add.push((
                                edge.source(), edge.target(), false,
                                Position::RegexPos(i.tau.clone(), i.k, false)
                            ));
                        }
                    },
                    None => {}
                }
            }
        }
        for (start,end,first,pos) in add.into_iter() {
            let weight = dag.edge_weight_mut(
                dag.find_edge(start, end).unwrap()).unwrap();
            weight.add_tk_pos(pos, first);
        }

        InputDataGraph { dag: dag }
    }
}

fn lowest_edge_cost(programs: &Edge) -> (usize, Option<Program>) {
    let mut min_cost = 0;
    let mut min_prog = None;
    for p in programs.programs.iter() {
        let cost = match p {
            Program::ConstantStr(_) => 1,
            Program::SubStr(p1, p2) => {
                let p1cost = match p1 {
                    Position::ConstantPos(_) => 1,
                    Position::RegexPos(_, _, _) => 2,
                };
                let p2cost = match p2 {
                    Position::ConstantPos(_) => 1,
                    Position::RegexPos(_, _, _) => 2,
                };
                p1cost + p2cost
            },
            Program::Concat(_) => panic!("Found a Concat on an edge!"),
        };
        if min_prog == None || min_cost > cost {
            min_cost = cost;
            min_prog = Some(p.clone());
        }
    }
    (min_cost, min_prog)
}

fn extract(odg: &InputDataGraph<Edge>, i: NodeIndex) -> (usize, Option<Vec<Program>>) {
    for x in odg.dag.children(i).iter(&odg.dag) {
        if odg.dag.node_weight(x.1).unwrap().first().unwrap().is_last() {
            let (cost, prog) = lowest_edge_cost(odg.dag.edge_weight(x.0).unwrap());
            return match prog {
                Some(p) => (cost, Some(vec![p])),
                None => (0, None)
            };
        }
    }
    let mut min_cost = 1;
    let mut min_prog = None;
    for x in odg.dag.children(i).iter(&odg.dag) {
        let branch_cost = extract(odg, x.1);
        if min_prog == None || min_cost > branch_cost.0 {
            min_cost = 1 + branch_cost.0;
            min_prog = branch_cost.1;
        }
    }

    (min_cost, min_prog)
}

pub fn gen_program(input: &'static Vec<String>, ncols: usize) -> Option<Program> {
    let outputs: Vec<String> = input.iter().skip(ncols - 1).step_by(ncols).cloned().collect();
    
    let mut threads = vec![];
    for i in 0..ncols-1 {
        let col: Vec<String> = input.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(|| InputDataGraph::gen_graph_column(col, true)));
    }
    let idgs: Vec<InputDataGraph<HashSet<PMatch>>> = threads
        .into_iter()
        .map(|x| x.join().unwrap())
        .collect();

    let mut odg = InputDataGraph::<Edge>::new(&outputs[0], &idgs);
    for i in outputs.iter().skip(1) {
        odg = odg.intersection(InputDataGraph::<Edge>::new(i, &idgs), true);
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
        match extract(&odg, NodeIndex::new(index)).1 {
            None => None,
            Some(v) => Some(Program::Concat(v))
        }
    }
}