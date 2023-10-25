mod inputdatagraph;

use inputdatagraph::gen_input_data_graph;
use once_cell::sync::Lazy;
use std::fs::read_to_string;
use std::env;

/*
 * Get input from file. Columns should be semicolon delimited. 1 row per line
 */
static INPUT: Lazy<Vec<String>> = Lazy::new(|| {
    let mut out: Vec<String> = vec![];
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(file) => {
            println!("{}",file);
            for s in read_to_string(file).unwrap().lines() {
                let mut x: Vec<String> = String::from(s).split(";").map(|x| String::from(x)).collect();
                out.append(&mut x);
            }
        }
        None => {
            panic!("Need input file");
        }
    }

    

    out
});

fn main() {
    let gs = gen_input_data_graph(&INPUT, 3);
    for n in 0..gs.len() {
        let mut fname = String::from("g");
        fname.push_str(n.to_string().as_str());
        fname.push_str(".dot");
        gs[n].to_dot(fname.as_str());
    }

}
