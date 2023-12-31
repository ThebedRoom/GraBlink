mod dsl;
mod enumerative;
mod inputdatagraph;
mod synthesizer;
mod vsa;

use enumerative::enumerative;
use env_logger;
use inputdatagraph::gen_input_data_graph;
use once_cell::sync::Lazy;
use std::env;
use std::fmt::Display;
use std::fs::read_to_string;
use std::time::Instant;
use synthesizer::Synthesizer;
use vsa::gen_program;

/**
 * Print usage info
 */
fn usage() {
    println!(
        r#"./grablink --egraph --vsa --enum --output-idg-to <FILE> -n <N> <INPUT>
    INPUT                 : input file (e.g. csv file).
    -n N                  : number of columns in input.
    --egraph              : Sets synthesis method to egraph
    --vsa                 : Sets synthesis method to vsa
    --vsan                : Sets synthesis method to vsa with numbers
    --enum                : Sets synthesis method to enumeration (default)
    --output-idg-to <FILE>: outputs idgs to FILE
    --time                : run all 3 methods and time them
    --timen               : run all 3 methods with numbers and time them"#
    );
}

/// Specifies which synthesizer backend to run
#[derive(PartialEq, Clone, Copy)]
enum SearchStrategy {
    EGRAPH,
    VSA,
    ENUMERATIVE,
    NONE,
}

impl Display for SearchStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SearchStrategy::EGRAPH => "EGRAPH",
                SearchStrategy::VSA => "VSA",
                SearchStrategy::ENUMERATIVE => "ENUMERATIVE",
                SearchStrategy::NONE => "NONE",
            }
        )
    }
}

struct Flags {
    search_strategy: SearchStrategy,
    output_inputdatagraph: bool,
    output_idg_file_prefix: Box<String>,
    input_file: Box<String>,
    column_count: usize,
    time: bool,
    with_nums: bool,
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
        }
        Err(e) => {
            usage();
            panic!("{}", e);
        }
    }
});

fn parse_args() -> Result<Flags, String> {
    let args: Vec<String> = env::args().collect();
    let mut flags = Flags {
        search_strategy: SearchStrategy::NONE,
        output_inputdatagraph: false,
        output_idg_file_prefix: Box::new(String::new()),
        input_file: Box::new(String::new()),
        column_count: 2,
        time: false,
        with_nums: false,
    };
    let mut apply_to_next: Option<fn(&mut Flags, String)> = None;

    for arg in args.into_iter().skip(1) {
        match apply_to_next {
            Some(f) => {
                f(&mut flags, arg);
                apply_to_next = None;
            }
            None => {
                if arg == "--egraph" {
                    flags.search_strategy = SearchStrategy::EGRAPH;
                } else if arg == "--vsa" {
                    flags.search_strategy = SearchStrategy::VSA;
                } else if arg == "--vsan" {
                    flags.search_strategy = SearchStrategy::VSA;
                    flags.with_nums = true;
                } else if arg == "--enum" {
                    flags.search_strategy = SearchStrategy::ENUMERATIVE;
                } else if arg == "--output-idg-to" {
                    flags.output_inputdatagraph = true;
                    apply_to_next = Some(|f, x| {
                        f.output_idg_file_prefix = Box::from(x);
                    });
                } else if arg == "-n" {
                    apply_to_next = Some(|f, x| {
                        f.column_count = match x.parse() {
                            Ok(n) => n,
                            Err(_) => 1,
                        }
                    });
                } else if arg == "--time" {
                    flags.time = true;
                } else if arg == "--timen" {
                    flags.time = true;
                    flags.with_nums = true;
                } else {
                    if !flags.input_file.is_empty() {
                        return Err(String::from("Two inputs specified!"));
                    }
                    flags.input_file = Box::from(arg);
                }
            }
        }
    }

    if flags.input_file.is_empty() {
        return Err(String::from("Missing input file!"));
    }

    Ok(flags)
}

fn synthesize_program(strategy: SearchStrategy) {
    let flags = &ARGS.0;
    let mut data_graphs = vec![];
    match strategy {
        SearchStrategy::EGRAPH => {
            data_graphs = gen_input_data_graph(&ARGS.1, flags.column_count, false, true);

            // currently expecting 2 columns
            let examples: Vec<(String, String)> = ARGS
                .1
                .chunks(2)
                .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
                .collect();

            let syn = Synthesizer::new(&examples, &data_graphs[0], &data_graphs[1]);
            match syn.synthesize("(!NONTERMINAL_E)") {
                Some((expr, cost)) => println!("{}\ncost = {}", expr, cost),
                None => println!("Synthesizedn't"),
            }
        }
        SearchStrategy::VSA => {
            let output = if flags.output_inputdatagraph { Some(*ARGS.0.output_idg_file_prefix.to_owned()) } else { None };
            let program = gen_program(
                &ARGS.1,
                ARGS.0.column_count,
                &output, flags.with_nums,
            );
            match program {
                Some(p) => {
                    println!("{}", p);
                }
                None => {
                    println!("Program could not be synthesized!");
                }
            }
        }
        SearchStrategy::ENUMERATIVE => {
            data_graphs = gen_input_data_graph(&ARGS.1, flags.column_count, false, true);

            let program = enumerative(&ARGS.1, &data_graphs[0], &data_graphs[1]);
            match program {
                Some(p) => {
                    println!("{:#?}", p);
                }
                None => {
                    println!("Program could not be synthesized!");
                }
            }
        }
        _ => {
            panic!("Need to specify a search strategy!");
        }
    }
    if flags.output_inputdatagraph {
        for n in 0..data_graphs.len() {
            let mut fname = ARGS.0.output_idg_file_prefix.to_owned();
            fname.push_str(n.to_string().as_str());
            fname.push_str(".dot");
            data_graphs[n].to_dot(fname.as_str(), false);
        }
    }
}

fn main() {
    env_logger::init();
    let flags = &ARGS.0;
    if flags.time && flags.search_strategy == SearchStrategy::NONE {
        for strategy in vec![
            SearchStrategy::EGRAPH,
            SearchStrategy::VSA,
            SearchStrategy::ENUMERATIVE,
        ]
        .iter()
        {
            let start = Instant::now();
            synthesize_program(*strategy);
            let elapsed = start.elapsed();
            println!(
                "Strategy {} took {}ms\n",
                strategy,
                elapsed.as_nanos() as f64 / 1000000.0
            )
        }
    } else if flags.time {
        let start = Instant::now();
        synthesize_program(flags.search_strategy);
        let elapsed = start.elapsed();
        println!(
            "Strategy {} took {}ms\n",
            flags.search_strategy,
            elapsed.as_nanos() as f64 / 1000000.0
        )
    } else {
        synthesize_program(flags.search_strategy)
    }
}
