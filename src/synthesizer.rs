use crate::dsl::{BlinkFillDSL, DSLInterpreter};
use crate::inputdatagraph::{InputDataGraph, PMatch};
use egg::*;

pub struct Synthesizer<'a> {
    examples: Vec<(String, String)>,
    idg: &'a InputDataGraph,
    max_concat_arity: usize,
    iter_limit: usize,
}

impl Synthesizer<'_> {
    pub fn new(
        examples: Vec<(String, String)>,
        idg: &InputDataGraph,
        max_concat_arity: usize,
        iter_limit: usize,
    ) -> Synthesizer {
        Synthesizer {
            examples,
            idg,
            max_concat_arity,
            iter_limit,
        }
    }

    pub fn synthesize(&self, s: &str) -> RecExpr<BlinkFillDSL> {
        // parse s into BlinkFillDSL
        let expr: RecExpr<BlinkFillDSL> = s.parse().unwrap();
        // Generate EGraph and run rewrite rules
        let runner = Runner::default()
            .with_iter_limit(self.iter_limit)
            .with_expr(&expr)
            .run(&self.make_rules());

        let cost = SynthesisCost {
            examples: &self.examples,
            egraph: &runner.egraph,
        };

        // Create an extractor with our custom cost function
        let extractor = Extractor::new(&runner.egraph, cost);
        // Extract lowest cost expression
        let (best_cost, best) = extractor.find_best(runner.roots[0]);
        println!("{} expanded to {} with cost {}", expr, best, best_cost);
        runner.egraph.dot().to_png("./egraph.png").unwrap();
        best // return best expr
    }

    fn make_rules(&self) -> Vec<Rewrite<BlinkFillDSL, ()>> {
        let mut rules: Vec<Rewrite<BlinkFillDSL, ()>> = Vec::new();

        // dynamic concat rules
        rules.extend((1..self.max_concat_arity + 1).map(|n| {
            let rule: Pattern<BlinkFillDSL> =
                format!("(!NONTERMINAL_CONCAT {})", "!NONTERMINAL_F ".repeat(n))
                    .parse()
                    .unwrap();
            rewrite!(format!("concat {}", n); "!NONTERMINAL_E" => rule)
        }));

        // idg gives us a set of pmatches to make dynamic rewrites for
        // for all pmatches: P -> pmatch (Start) | pmatch (End)
        let pmatches = self.get_pmatch_set();
        for (i, pmatch) in pmatches.iter().enumerate() {
            if pmatch.constantstr {
                // This is absoLUTEly going to cause an issue if the constant string happens to also be a grammar production
                let rule: Pattern<BlinkFillDSL> = pmatch.tau.parse().unwrap();
                rules.push(rewrite!(format!("pmatch {}", i); "!NONTERMINAL_F" => rule));
            } else {
                let rule: Pattern<BlinkFillDSL> = format!(
                    "(!TERMINAL_POS {} {} {})",
                    pmatch.tau,
                    pmatch.k,
                    "!NONTERMINAL_DIR"
                )
                .parse()
                .unwrap();
                rules.push(rewrite!(format!("pmatch {}", i); "!NONTERMINAL_P" => rule));
            }
        }

        // static rules
        rules.extend(vec![
            rewrite!("F -> substr(vi, pl, pr)"; "(!NONTERMINAL_F)" => 
                "(!NONTERMINAL_SUBSTR !TERMINAL_INPUT !NONTERMINAL_P !NONTERMINAL_P)"),
            rewrite!("Dir -> Start"; "(!NONTERMINAL_DIR)" => "!TERMINAL_START"),
            rewrite!("Dir -> END"; "(!NONTERMINAL_DIR)" => "!TERMINAL_END"),
        ]);

        rules
    }

    fn get_pmatch_set(&self) -> Vec<&PMatch> {
        let edges = self.idg.dag.raw_edges();
        let mut matches: Vec<&PMatch> = Vec::new();
        for e in edges {
            let mch = e.weight.iter().next().unwrap(); // get random item
            matches.push(mch);
        }
        matches
    }
}

struct SynthesisCost<'a> {
    examples: &'a Vec<(String, String)>,
    egraph: &'a EGraph<BlinkFillDSL, ()>,
}
impl CostFunction<BlinkFillDSL> for SynthesisCost<'_> {
    type Cost = f64;
    // this function gives a cost for individual nodes.
    fn cost<C>(&mut self, enode: &BlinkFillDSL, mut _costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let mut cost = 0.0;
        match enode {
            BlinkFillDSL::E => { cost += 1_000_000.0; }
            BlinkFillDSL::F => { cost += 1_000_000.0; }
            BlinkFillDSL::P => { cost += 1_000_000.0; }
            BlinkFillDSL::Dir => { cost += 1_000_000.0; }
            BlinkFillDSL::Pos(ids) => {
                cost += _costs(ids[2]);
            }
            BlinkFillDSL::Concat(ids) => {
                for n in ids.iter() {
                    cost += _costs(*n);
                }
            }
            BlinkFillDSL::Substr(ids) => {
                cost += _costs(ids[1]) + _costs(ids[2]);
            }
            _ => {}
        }

        cost
    }

    // the extractor never calls this function
    fn cost_rec(&mut self, expr: &RecExpr<BlinkFillDSL>) -> Self::Cost {
        let mut cost = 0.0;
        println!("Synthesized: {}", expr.pretty(10));
        let intpr = DSLInterpreter::new(&expr);
        for (input, output) in self.examples {
            match intpr.interpret(input) {
                Some(o) => {
                    if o.to_string() != *output {
                        // program is not correct
                        cost += 1_000_000.0;
                    }
                }
                None => cost += 1_000_000.0, // program is not valid
            }
        }
        // optionally, we can have lower costs for shorter programs
        cost += expr.as_ref().len() as f64;

        cost
    }
}
