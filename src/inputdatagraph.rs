use once_cell::sync::Lazy;
use std::collections::hash_map::RandomState;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::fs;
use std::thread;

use daggy::{Dag, NodeIndex, EdgeIndex, Walker};
use regex::Regex;

/// Map of token names to regular expressions.
pub(crate) static TOKENS: Lazy<HashMap<&str, Regex>> = Lazy::new(|| {
    let mut tokens = HashMap::new();
    tokens.insert("ProperCase", Regex::new(r"[A-Z][a-z]+").unwrap());
    tokens.insert("CAPS", Regex::new(r"[A-Z]+").unwrap());
    tokens.insert("lowercase", Regex::new(r"[a-z]+").unwrap());
    tokens.insert("Digits", Regex::new(r"\d+").unwrap());
    tokens.insert("Alphabets", Regex::new(r"[a-zA-Z]+").unwrap());
    tokens.insert("Alphanumeric", Regex::new(r"[a-zA-Z0-9]+").unwrap());
    tokens.insert("Whitespace", Regex::new(r"\s+").unwrap());
    tokens.insert(
        "ProperCaseWSpaces",
        Regex::new(r"[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*").unwrap(),
    );
    tokens.insert("CAPSWSpaces", Regex::new(r"[A-Z]+(?:\s+[A-Z]+)*").unwrap());
    tokens.insert(
        "lowercaseWSpaces",
        Regex::new(r"[a-z]+(?:\s+[a-z]+)*").unwrap(),
    );
    tokens.insert(
        "AlphabetsWSpaces",
        Regex::new(r"[a-zA-Z]+(?:\s+[a-zA-Z]+)*").unwrap(),
    );
    tokens
});

/// Labels for edges
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct PMatch {
    /// Refers to a specific regex
    pub tau: String,
    /// `PMatch` instance refers to the kth instance of `tau` in a string
    pub k: i32,
    /// If tau is a constant string rather than a token
    pub constantstr: bool,
}

impl PMatch {
    fn new(tau: &str, k: i32, constantstr: bool) -> Self {
        return PMatch {
            tau: String::from(tau),
            k,
            constantstr
        };
    }
}

impl Display for PMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.tau, self.k, self.constantstr)
    }
}

/// Labels for nodes (ensures uniqueness during `InputDataGraph` intersections)
#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct NodeID {
    /// String used to generate the graph this ID is from
    s: String,
    /// Index of `s` this `NodeID` represents
    i: u32,
    /// How many extra nodes are generated in IDG
    offset: u32,
}

impl NodeID {
    pub fn new(s: String, i: u32, o: u32) -> Self {
        NodeID { s, i, offset: o }
    }

    pub fn is_first(&self) -> bool {
        self.i == 0
    }

    pub fn is_last(&self) -> bool {
        self.i as usize == self.s.len() + self.offset as usize
    }
}

impl Display for NodeID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.s, self.i)
    }
}

/**
 * `H` is an iterable that implements an `intersection`, `is_empty`, and `iter` method
 */
pub trait Intersectable<'a, H, S = RandomState> {
    /// The type stored in the iterable
    type Item: std::fmt::Display + PartialEq + Eq + std::hash::Hash + Clone;
    fn intersection(&'a self, other: &'a H) -> std::collections::hash_set::Intersection<'a, Self::Item, S>;
    fn is_empty(&self) -> bool;
    fn iter(&self) -> std::collections::hash_set::Iter<'_, Self::Item>;
}

impl<'a, T: std::fmt::Display + PartialEq + Eq + 
        std::hash::Hash + Clone> Intersectable<'a, HashSet<T>> for HashSet<T> {
    type Item = T;
    fn intersection(&'a self, other: &'a HashSet<T>) -> std::collections::hash_set::Intersection<'a, Self::Item, RandomState> {
        self.intersection(other)
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn iter(&self) -> std::collections::hash_set::Iter<'_, Self::Item> {
        self.iter()
    }
}

/// Wrapper for a daggy::Dag
pub struct InputDataGraph<H: for<'a> Intersectable<'a, H>> 
{
    /**
     * Nodes are weighted with hashable sets of `NodeID`s to ensure uniqueness.
     */
    pub dag: Dag<BTreeSet<NodeID>, H>,
}

impl<T: for<'a>Intersectable<'a, T> + Clone + 
        for<'a>std::iter::FromIterator<<T as Intersectable<'a, T>>::Item>> InputDataGraph<T> 
{
    /**
     * Finds the intersection of `self` and `other`
     */
    pub fn intersection(&self, other: InputDataGraph<T>, with_deletions: bool) -> InputDataGraph<T> {
        let mut dag: Dag<BTreeSet<NodeID>, T> = Dag::new();
        // used for preventing duplicate nodes
        let mut nmap: HashMap<BTreeSet<NodeID>, NodeIndex> = HashMap::new();

        // generate all nodes and edges based on edge label intersections
        for e1 in self.dag.raw_edges() {
            for e2 in other.dag.raw_edges() {
                let i: T = e1.weight.intersection(&e2.weight).cloned().collect();
                if i.is_empty() {
                    continue;
                }
                let sn1set = self.dag.node_weight(e1.source()).unwrap();
                let sn2set = other.dag.node_weight(e2.source()).unwrap();
                let iset: BTreeSet<NodeID> = sn1set.union(&sn2set).cloned().collect();
                let sindex: NodeIndex;
                if nmap.contains_key(&iset) {
                    sindex = nmap.get(&iset).cloned().unwrap();
                } else {
                    sindex = dag.add_node(iset.clone());
                    nmap.insert(iset, sindex.clone());
                }
                let tn1set = self.dag.node_weight(e1.target()).unwrap();
                let tn2set = other.dag.node_weight(e2.target()).unwrap();
                let iset: BTreeSet<NodeID> = tn1set.union(&tn2set).cloned().collect();
                let tindex: NodeIndex;
                if nmap.contains_key(&iset) {
                    tindex = nmap.get(&iset).cloned().unwrap();
                } else {
                    tindex = dag.add_node(iset.clone());
                    nmap.insert(iset, tindex.clone());
                }
                let _ = dag.add_edge(sindex, tindex, i);
            }
        }

        let mut out = InputDataGraph { dag: dag };
        if with_deletions { out.remove_dead_unreachable(); }
        out
    }

    /**
     * Removes nodes and edges that cannot be reached from the start state, or cannot reach the final state
     */
    fn remove_dead_unreachable(&mut self) {
        'outer: loop {
            for n in 0..self.dag.node_count() {
                let i = NodeIndex::new(n);
                if self.is_dead_or_unreachable(i) {
                    self.dag.remove_node(i);
                    continue 'outer;
                }
            }
            break;
        }
    }

    /**
     * Checks if a node is is dead or unreachable
     */
    fn is_dead_or_unreachable(&self, node: NodeIndex) -> bool {
        let ids = self.dag.node_weight(node).unwrap();
        let id = ids.first().unwrap();
        let parents: HashMap<EdgeIndex, NodeIndex> = 
            self.dag.parents(node)
            .iter(&self.dag)
            .collect();
        let children: HashMap<EdgeIndex, NodeIndex> = 
            self.dag.children(node)
            .iter(&self.dag)
            .collect();
        (!id.is_first() && parents.len() == 0) || (!id.is_last() && children.len() == 0)
    }

    /**
     * Outputs graph in `graphviz` format to `file`
     */
    pub fn to_dot(&self, file: &str, show_node_ids: bool) {
        let mut data =
            String::from("digraph g {\nratio=\"compress\"\nnode [shape=rectangle]\nrankdir=\"LR\"\n\nsubgraph gg{\n\n");

        for e in self.dag.raw_edges() {
            data.push('\t');
            if show_node_ids {
                let mut es = String::new();
                for nid in self.dag.node_weight(e.source()).unwrap().iter() {
                    es.push_str(nid.to_string().as_str());
                }
                let mut et = String::new();
                for nid in self.dag.node_weight(e.target()).unwrap().iter() {
                    et.push_str(nid.to_string().as_str());
                }
                data.push_str(format!(
                    "{} [label=\"{}\"]\n{} [label=\"{}\"]\n",
                    e.source().index(),
                    es,
                    e.target().index(),
                    et
                ).as_str());
            }
            let mut label = String::new();
            for s in e.weight.iter() {
                label.push_str(format!("{}, ",s.to_string()).as_str());
            }
            data.push_str(format!(
                "\t{} -> {} [label=\"{}\"]\n",
                e.source().index(),
                e.target().index(),
                label
            ).as_str());
            
        }

        data.push_str("}\n}\n");
        fs::write(file, data).expect("Unable to write to file");
    }
}

impl InputDataGraph<HashSet<PMatch>> {
    /**
     * Generates a dag based on `s`, with positive and negative indexed `PMatch` edges for all
     * substrings of `s` and regexes in `TOKENS`
     */
    pub fn new(s: &String, with_conststr: bool) -> Self {
        // init dag
        let n = s.len();
        let mut dag: Dag<BTreeSet<NodeID>, HashSet<PMatch>> =
            Dag::with_capacity(n + 3, ((n * (n + 1)) / 2) + 2);
        for i in 0..n as u32 + 3 {
            dag.add_node(BTreeSet::from([NodeID::new(s.clone(), i, 2)]));
        }

        let mut matchcount: HashMap<String, i32> = HashMap::new();

        // default edges
        *matchcount.entry(String::from("StartT")).or_default() += 1;
        *matchcount.entry(String::from("EndT")).or_default() += 1;
        let _ = dag.add_edge(
            NodeIndex::new(0),
            NodeIndex::new(1),
            [PMatch::new("StartT", 1, false)].into(),
        );
        let _ = dag.add_edge(
            NodeIndex::new(s.len() + 1),
            NodeIndex::new(s.len() + 2),
            [PMatch::new("EndT", 1, false)].into(),
        );

        // create substring edge labels
        for i in 1..s.len() + 1 {
            for j in i + 1..s.len() + 2 {
                let substring = &s[i - 1..j - 1];
                *matchcount.entry(substring.to_string()).or_default() += 1;
                let _ = dag.add_edge(
                    NodeIndex::new(i),
                    NodeIndex::new(j),
                    if with_conststr {
                        [PMatch::new(
                            substring, 
                            *matchcount.get(substring).unwrap(),
                            true
                        )].into()
                    } else { 
                        HashSet::new()
                    },
                );
            }
        }

        // token matches
        for (t, r) in TOKENS.iter() {
            *matchcount.entry(String::from(*t)).or_default() = 0;
            for needle in r.find_iter(s.as_str()) {
                let ns = NodeIndex::new(needle.start() + 1);
                let ne = NodeIndex::new(needle.end() + 1);
                let edge = dag.find_edge(ns, ne).unwrap();
                let kstr = String::from(*t);
                *matchcount.entry(kstr.clone()).or_default() += 1;

                // Union the token match with the current set of tokens for this edge
                let uniondweight = dag
                    .edge_weight(edge)
                    .unwrap()
                    .union(&HashSet::from_iter([
                        PMatch::new(
                            *t,
                            *matchcount.get(&kstr).unwrap(),
                            false
                        )
                    ]))
                    .cloned()
                    .collect();
                let _ = dag.update_edge(ns, ne, uniondweight);
            }
        }

        // create negative labels
        let mut add: Vec<(NodeIndex, NodeIndex, HashSet<PMatch>)> = vec![];
        for e in dag.raw_edges() {
            let mut negmatches = HashSet::new();
            for m in e.weight.iter() {
                let mc = matchcount.get(&m.tau).unwrap();
                for i in 1..mc + 1 {
                    if m.k == mc - (i - 1) {
                        negmatches.insert(PMatch::new(&m.tau, -i, m.constantstr));
                    }
                }
            }
            add.push((
                e.source(),
                e.target(),
                e.weight.union(&negmatches).cloned().collect(),
            ));
        }

        for e in add {
            let _ = dag.update_edge(e.0, e.1, e.2);
        }

        InputDataGraph { dag: dag }
    }

    /**
     * Generates a dag based on a column of strings `col`, with positive and negative indexed `PMatch` edges for all
     * substrings of `s` and regexes in `TOKENS`
     */
    pub fn gen_graph_column(col: Vec<String>, with_deletions: bool, with_conststr: bool) -> InputDataGraph<HashSet<PMatch>> {
        if col.len() == 0 {
            panic!("Cannot generate graph on an empty column")
        }
        let mut g = InputDataGraph::new(&col[0].to_string(), with_conststr);
        for i in col.iter().skip(1) {
            g = g.intersection(InputDataGraph::new(&i.to_string(), with_conststr), with_deletions);
        }
        g
        // let mut threads = vec![];

        // for i in col.into_iter() {
        //     threads.push(thread::spawn(|| {
        //         InputDataGraph::new(i)
        //     }));
        // }

        // let mut g: Option<InputDataGraph> = None;

        // for i in threads.into_iter() {
        //     match g {
        //         Some(graph) => { g = Some(graph.intersection(i.join().unwrap())) }
        //         None => { g = Some(i.join().unwrap()); }
        //     }
        // }
        // g.unwrap()
    }
}

/**
 * Generates an `InputDataGraph` for each column from the vector of concatenated `rows`
 */
pub fn gen_input_data_graph(rows: &'static Vec<String>, ncols: usize, with_deletions: bool, with_conststr: bool) -> Vec<InputDataGraph<HashSet<PMatch>>> {
    // let mut out = vec![];
    // for i in 0..ncols {
    //     out.push(InputDataGraph::gen_graph_column(rows.iter().skip(i).step_by(ncols).cloned().collect()));
    // }
    // out
    let mut threads = vec![];
    if rows.len() % ncols != 0 {
        panic!("Rows are not all the same length!");
    }
    for i in 0..ncols {
        let col = rows.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(move || InputDataGraph::gen_graph_column(col, with_deletions, with_conststr)));
    }
    threads.into_iter().map(|x| x.join().unwrap()).collect()
}
