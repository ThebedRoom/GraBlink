mod dsl;
mod inputdatagraph;
mod synthesizer;
mod vsa;

use inputdatagraph::gen_input_data_graph;
use once_cell::sync::Lazy;
use std::env;
use std::fs::read_to_string;
use synthesizer::Synthesizer;
use vsa::gen_program;

/**
 * Print usage info
 */
fn usage() {
    println!(r#"./grablink --egraph --vsa --enum -n <N> <INPUT>
    INPUT   : input file (e.g. csv file).
    -n N    : number of columns in input.
    --egraph: Sets synthesis method to egraph
    --vsa   : Sets synthesis method to vsa
    --enum  : Sets synthesis method to enumeration (default)"#);
}

/// Specifies which synthesizer backend to run
enum SearchStrategy {
    EGRAPH, VSA, ENUMERATIVE
}

struct Flags {
    search_strategy: SearchStrategy,
    output_inputdatagraph: bool,
    output_idg_file_prefix: Box<String>,
    input_file: Box<String>,
    column_count: usize,
}

/**
 * Contains cmd line flags and also the contents of the input file. 
 */
static ARGS: Lazy<(Flags, Vec<String>)> = Lazy::new(|| {
    let args = parse_args();
    match args {
        Ok(flags) => {
            let mut input = vec![];
            for s in read_to_string(flags.input_file.as_str()).unwrap().lines() {
                let mut x: Vec<String> = String::from(s)
                    .split(";")
                    .map(|x| String::from(x))
                    .collect();
                input.append(&mut x);
            }

            (flags, input)
        },
        Err(e) => {
            usage();
            panic!("{}",e);
        }
    }
});

fn parse_args() -> Result<Flags, String> {
    let args: Vec<String> = env::args().collect();
    let mut flags = Flags {
        search_strategy: SearchStrategy::ENUMERATIVE,
        output_inputdatagraph: false,
        output_idg_file_prefix: Box::new(String::new()),
        input_file: Box::new(String::new()),
        column_count: 1
    };
    let mut apply_to_next: Option<fn(&mut Flags, String)> = None;

    for arg in args.into_iter().skip(1) {
        match apply_to_next {
            Some(f) => {
                f(&mut flags, arg);
                apply_to_next = None;
            }
            None => {
                if arg == "--egraph" { flags.search_strategy = SearchStrategy::EGRAPH; }
                else if arg == "--vsa" { flags.search_strategy = SearchStrategy::VSA; }
                else if arg == "--enum" { flags.search_strategy = SearchStrategy::ENUMERATIVE; }
                else if arg == "--output-idg-to" {
                    flags.output_inputdatagraph = true;
                    apply_to_next = Some(|f, x| { 
                        f.output_idg_file_prefix = Box::from(x); 
                    });
                }
                else if arg == "-n" {
                    apply_to_next = Some(|f,x| { 
                        f.column_count = match x.parse() {
                            Ok(n) => n,
                            Err(_) => 1
                        }
                    });
                }
                else { 
                    if !flags.input_file.is_empty() {
                        return Err(String::from("Two inputs specified!"));
                    }
                    flags.input_file = Box::from(arg); 
                }
            }
        }
    }

    if flags.input_file.is_empty() {
        return Err(String::from("Missing input file!"))
    }

    Ok(flags)
}

fn main() {
    let flags = &ARGS.0;

    let data_graphs = gen_input_data_graph(&ARGS.1, flags.column_count, true);
    
    if flags.output_inputdatagraph {
        for n in 0..data_graphs.len() {
            let mut fname = String::from("g");
            fname.push_str(n.to_string().as_str());
            fname.push_str(".dot");
            data_graphs[n].to_dot(fname.as_str(), false);
        }
    }

    match flags.search_strategy {
        SearchStrategy::EGRAPH => {
            // currently expecting 2 columns
            let examples: Vec<(String, String)> = ARGS.1
                .chunks(2)
                .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
                .collect();
        
            let syn = Synthesizer::new(examples, &data_graphs[0], 1, 100);
            let expr = syn.synthesize("(!NONTERMINAL_E)");
            println!("{}", expr.pretty(10));
        }
        SearchStrategy::VSA => {
            let program = gen_program(&ARGS.1, ARGS.0.column_count);
            match program {
                Some(p) => { println!("{}",p); }
                None => { println!("Program could not be synthesized!"); }
            }
        }
        // TODO: implement rest
        _ => {}
    }

}
