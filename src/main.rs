mod dsl;
mod inputdatagraph;
mod synthesizer;

use inputdatagraph::gen_input_data_graph;
use once_cell::sync::Lazy;
use std::env;
use std::fs::read_to_string;
use synthesizer::Synthesizer;

/**
 * Print usage info
 */
fn usage() {
    println!("./grablink <INPUT> <N>\n\tINPUT: input file (e.g. csv file)\n\tN   : number of columns in input");
}

/*
 * Get input from file. Columns should be semicolon delimited. 1 row per line
 */
static INPUT: Lazy<Vec<String>> = Lazy::new(|| {
    let mut out: Vec<String> = vec![];
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(file) => {
            println!("{}", file);
            for s in read_to_string(file).unwrap().lines() {
                let mut x: Vec<String> = String::from(s)
                    .split(";")
                    .map(|x| String::from(x))
                    .collect();
                out.append(&mut x);
            }
        }
        None => {
            usage();
            panic!("Need input file");
        }
    }

    out
});

fn main() {
    let args: Vec<String> = env::args().collect();
    let ncols: usize = if let Some(s) = args.get(2) {
        s.parse().unwrap_or(1)
    } else {
        usage();
        panic!("Need number of columns");
    };

    let gs = gen_input_data_graph(&INPUT, ncols);
    for n in 0..gs.len() {
        let mut fname = String::from("g");
        fname.push_str(n.to_string().as_str());
        fname.push_str(".dot");
        gs[n].to_dot(fname.as_str());
    }

    // currently expecting 2 columns
    let examples: Vec<(String, String)> = INPUT
        .chunks(2)
        .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
        .collect();

    let syn = Synthesizer::new(examples, &gs[1], 1, 100);
    let expr = syn.synthesize("(!NONTERMINAL_E)");

    println!("{}", expr.pretty(10));
}
