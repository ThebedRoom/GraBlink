use std::collections::{HashSet, HashMap, BTreeSet};
use std::fmt::Display;
use std::fs;
use once_cell::sync::Lazy;
use std::thread;

use regex::Regex;
use daggy::{Dag, NodeIndex, Walker};

/// Map of token names to regular expressions.
static TOKENS: Lazy<HashMap<&str, Regex>> = Lazy::new(|| {
    let mut tokens = HashMap::new();
    tokens.insert("ProperCase", Regex::new(r"[A-Z][a-z]+").unwrap());
    tokens.insert("CAPS", Regex::new(r"[A-Z]+").unwrap());
    tokens.insert("lowercase", Regex::new(r"[a-z]+").unwrap());
    tokens.insert("Digits", Regex::new(r"\d+").unwrap());
    tokens.insert("Alphabets", Regex::new(r"[a-zA-Z]+").unwrap());
    tokens.insert("Alphanumeric", Regex::new(r"[a-zA-Z0-9]+").unwrap());
    tokens.insert("Whitespace", Regex::new(r"\s+").unwrap());
    tokens.insert("ProperCaseWSpaces", Regex::new(r"[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*").unwrap());
    tokens.insert("CAPSWSpaces", Regex::new(r"[A-Z]+(?:\s+[A-Z]+)*").unwrap());
    tokens.insert("lowercaseWSpaces", Regex::new(r"[a-z]+(?:\s+[a-z]+)*").unwrap());
    tokens.insert("AlphabetsWSpaces", Regex::new(r"[a-zA-Z]+(?:\s+[a-zA-Z]+)*").unwrap());
    tokens
});

/// Labels for edges 
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct PMatch {
    /// Refers to a specific regex
    tau: String,
    /// `PMatch` instance refers to the kth instance of `tau` in a string
    k: i32,
}

impl PMatch {
    fn new(tau: &str, k: i32) -> Self {
        return PMatch { tau: String::from(tau), k }
    }
}

impl Display for PMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(\"{}\", {})", self.tau, self.k)
    }
}

/// Labels for nodes (ensures uniqueness during `InputDataGraph` intersections)
#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct NodeID {
    /// String used to generate the graph this ID is from
    s: String,
    /// Index of `s` this `NodeID` represents
    i: u32,
}

impl NodeID {
    pub fn new(s: String, i: u32) -> Self {
        NodeID { s, i }
    }
}

/// Wrapper for a daggy::Dag
pub struct InputDataGraph {
    /**
     * Nodes are weighted with hashable sets of `NodeID`s to ensure uniqueness.
     * Edges are weighted with sets of `PMatch`.
     */
    pub dag: Dag<BTreeSet<NodeID>, HashSet<PMatch>>
}

impl InputDataGraph {
    /** 
     * Generates a dag based on `s`, with positive and negative indexed `PMatch` edges for all
     * substrings of `s` and regexes in `TOKENS`
     */
    pub fn new(s: String) -> Self {
        // init dag
        let n = s.len();
        let mut dag: Dag<BTreeSet<NodeID>, HashSet<PMatch>> = Dag::with_capacity(n+3, ((n*(n+1))/2)+2);
        for i in 0..n as u32 + 3 {
            dag.add_node(BTreeSet::from([NodeID::new(s.clone(), i)]));
        }

        let mut matchcount: HashMap<String, i32> = HashMap::new();
        *matchcount.entry(String::from("StartT")).or_default() += 1;
        *matchcount.entry(String::from("EndT")).or_default() += 1;
        let mut hset;

        // default edges
        hset = [PMatch::new("StartT", 1)].iter().cloned().collect();
        let _ = dag.add_edge(NodeIndex::new(0), NodeIndex::new(1), hset);
        hset = [PMatch::new("EndT", 1)].iter().cloned().collect();
        let _ = dag.add_edge(NodeIndex::new(s.len()+1), NodeIndex::new(s.len()+2), hset);

        // create substring edge labels
        for i in 1..s.len()+1 {
            for j in i+1..s.len()+2 {
                let substring = &s[i-1..j-1];
                let mstr = String::from("c_".to_owned() + substring);
                *matchcount.entry(mstr.clone()).or_default() += 1;
                hset = [PMatch::new(mstr.as_str(), *matchcount.get(&mstr).unwrap())].iter().cloned().collect();
                let _ = dag.add_edge(NodeIndex::new(i), NodeIndex::new(j), hset);
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
                let mut h = HashSet::new();
                h.insert(PMatch::new(*t, *matchcount.get(&kstr).unwrap()));
                let b = dag.edge_weight(edge).unwrap().union(&h.iter().cloned().collect()).cloned().collect();
                let _ = dag.update_edge(ns, ne, b);
            }
        }

        let mut add: Vec<(NodeIndex, NodeIndex, HashSet<PMatch>)> = vec![];
        for e in add.into_iter() {
            let _ = dag.update_edge(e.0, e.1, e.2);
        }

        // create negative labels
        add = vec![];
        for e in dag.raw_edges() {
            let mut ps = HashSet::new();
            for m in e.weight.iter() {
                let mc = matchcount.get(&m.tau).unwrap();
                for i in 1..mc+1 {
                    if m.k == mc - (i - 1) {
                        ps.insert(PMatch::new(&m.tau, -i));
                    }
                }
            }
            let b = e.weight.union(&ps.iter().cloned().collect()).cloned().collect();
            add.push((e.source(), e.target(), b));
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
    pub fn gen_graph_column(col: Vec<String>) -> InputDataGraph {
        if col.len() == 0 { panic!("Cannot generate graph on an empty column") }
        let mut g = InputDataGraph::new(col[0].to_string());
        for i in col.iter().skip(1) {
            g = g.intersection(InputDataGraph::new(i.to_string()));
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

    /** 
     * Finds the intersection of `self` and `other`
     */
    fn intersection(&self, other: InputDataGraph) -> InputDataGraph {
        let mut dag: Dag<BTreeSet<NodeID>, HashSet<PMatch>> = Dag::new();
        // used for preventing duplicate nodes
        let mut nmap: HashMap<BTreeSet<NodeID>, NodeIndex> = HashMap::new();
        
        // generate all nodes and edges based on edge label intersections
        for e1 in self.dag.raw_edges() {
            for e2 in other.dag.raw_edges() {
                let i: HashSet<PMatch> = e1.weight.intersection(&e2.weight).cloned().collect();
                if i.is_empty() { continue }
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
        out.remove_dead_unreachable();
        out
    }

    /**
     * Outputs graph in `graphviz` format to `file`
     */
    pub fn to_dot(&self, file: &str) {
        let mut data = String::from("digraph g {\nnode [shape=rectangle]\nrankdir=\"LR\"\n\nsubgraph gg{\n\n");
    
        for e in self.dag.raw_edges() {
            data.push('\t');
            data.push_str(e.source().index().to_string().as_str());
            data.push_str(" -> ");
            data.push_str(e.target().index().to_string().as_str());
            data.push_str(" [label=\"");
            for s in e.weight.iter() {
                data.push('(');
                data.push_str(s.tau.as_str());
                data.push(',');
                data.push_str(s.k.to_string().as_str());
                data.push(')');
            }
            data.push_str("\"]\n")
        }
    
        data.push_str("}\n}\n");
        fs::write(file,data).expect("Unable to write to file");
    }

    /**
     * Removes nodes and edges that cannot be reached from the start state, or cannot reach the final state
     */
    fn remove_dead_unreachable(&mut self) {
        'outer: loop {
            for n in 0..self.dag.node_count()  {
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
        // check if parent edge is final edge
        let mut has_parents = false;
        for (e, _) in self.dag.parents(node).iter(&self.dag) {
            has_parents = true;
            if self.dag.edge_weight(e).unwrap().contains(&PMatch::new("EndT", 1)) { return false; }
        }
        // check if children edge are final edges, or recurse
        let mut out = true;
        let mut has_start_edge = false;
        for (e, n) in self.dag.children(node).iter(&self.dag) {
            if self.dag.edge_weight(e).unwrap().contains(&PMatch::new("StartT", 1)) { has_start_edge = true }
            if out {
                if self.dag.edge_weight(e).unwrap().contains(&PMatch::new("EndT", 1)) { return false; }
                if !self.is_dead_or_unreachable(n) { 
                    out = false; // *could* return here, but 
                }
            }
        }

        out || (!has_parents && !has_start_edge)
    }
}

/**
 * Generates an `InputDataGraph` for each column from the vector of concatenated `rows`
 */
pub fn gen_input_data_graph(rows: &'static Vec<String>, ncols: usize) -> Vec<InputDataGraph> {
    // let mut out = vec![];
    // for i in 0..ncols {
    //     out.push(InputDataGraph::gen_graph_column(rows.iter().skip(i).step_by(ncols).cloned().collect()));
    // }
    // out
    let mut threads = vec![];
    if rows.len() % ncols != 0 { panic!("Rows are not all the same length!"); }
    for i in 0..ncols {
        let col = rows.iter().skip(i).step_by(ncols).cloned().collect();
        threads.push(thread::spawn(|| { 
            InputDataGraph::gen_graph_column(col) 
        }));
    }
    threads.into_iter().map(|x| x.join().unwrap()).collect()
}