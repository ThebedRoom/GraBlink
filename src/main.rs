mod dsl;
mod inputdatagraph;
mod synthesizer;

use dsl::*;
use egg::*;
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
    match args.get(2) {
        Some(x) => {
            let gs = gen_input_data_graph(&INPUT, x.parse::<usize>().unwrap_or(1));
            for n in 0..gs.len() {
                let mut fname = String::from("g");
                fname.push_str(n.to_string().as_str());
                fname.push_str(".dot");
                gs[n].to_dot(fname.as_str());
            }
        }
        None => {
            usage();
        }
    }

    // let syn = Synthesizer::new(vec!["".to_owned()], 10, 10);
    // let expr = syn.synthesize("(E)");

    // let expr_str = "(concat help me im stuck)";
    // let rec_expr: RecExpr<BlinkFillDSL> = expr_str.parse().unwrap();

    // println!("{}", rec_expr.pretty(10));

    // let intpr = DSLInterpreter::new(&rec_expr);
    // println!("{}", intpr.interpret(String::from("")).unwrap())
}
