use crate::dsl::{BlinkFillDSL, DSLInterpreter};
use crate::inputdatagraph::{InputDataGraph, PMatch};
use egg::*;
use itertools::Itertools;
use std::collections::HashSet;

pub struct Synthesizer<'a> {
    examples: &'a Vec<(String, String)>,
    idg: &'a InputDataGraph<HashSet<PMatch>>,
}

impl<'a> Synthesizer<'a> {
    pub fn new(
        examples: &'a Vec<(String, String)>,
        idg: &'a InputDataGraph<HashSet<PMatch>>,
    ) -> Synthesizer<'a> {
        Synthesizer { examples, idg }
    }

    /// Given some input string `s`, synthesize a program that works
    /// over all self.examples, based on information from self.idg.
    pub fn synthesize(&self, s: &str, early_break_on_fail: bool) -> (RecExpr<BlinkFillDSL>, f64) {
        let expr: RecExpr<BlinkFillDSL> = s.parse().unwrap();
        // max concat arity needed is max length of output
        let max_concat_arity = self.examples.iter().map(|e| e.1.len()).max().unwrap();

        // set initial bests
        let mut running_best_expr: RecExpr<BlinkFillDSL> = "e".parse().unwrap();
        let mut running_best_cost = f64::INFINITY;

        for arity in 1..max_concat_arity + 1 {
            // Generate EGraph and run rewrite rules
            let rules = self.make_rules(arity);
            let runner = Runner::default().with_expr(&expr).run(&rules);

            let cost = SynthesisCost {
                examples: &self.examples,
                egraph: &runner.egraph,
                early_break_on_fail,
            };

            // Create an extractor with our custom cost function
            let extractor = Extractor::new(&runner.egraph, cost);
            // Extract lowest cost expression
            let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

            if best_cost == 0.0 {
                return (best_expr, best_cost); // found a complete program
            }
            if best_cost < running_best_cost {
                // update if new best
                running_best_expr = best_expr;
                running_best_cost = best_cost;
            }
        }

        (running_best_expr, running_best_cost) // did not find correct, return most correct
    }

    /// Given a specific concat_arity, build the rewrite rules
    /// for all possible concat expressions of said arity.
    fn make_rules(&self, concat_arity: usize) -> Vec<Rewrite<BlinkFillDSL, ()>> {
        let mut rules: Vec<Rewrite<BlinkFillDSL, ()>> = Vec::new();

        // get set of all 2perms of pmatches, this gives all possible substrs
        let substrs = self.get_substr_set();
        let useful_substrs = self.filter_substrs(substrs);

        // get all permutations of length `concat_arity` of substrs
        let all_orderings = useful_substrs.into_iter().permutations(concat_arity);

        for (i, ordering) in all_orderings.enumerate() {
            // build up concat rewrite for this ordering
            let mut concat = String::from("(!NONTERMINAL_CONCAT ");
            for sub_pair in ordering {
                concat += &format!(
                    "(!NONTERMINAL_SUBSTR !TERMINAL_INPUT (!TERMINAL_POS \"{}\" {} {}) (!TERMINAL_POS \"{}\" {} {})) ",
                    sub_pair[0].0.tau, sub_pair[0].0.k, sub_pair[0].1, sub_pair[1].0.tau, sub_pair[1].0.k, sub_pair[1].1
                );
            }
            concat += ")";

            let concat_pattern: Pattern<BlinkFillDSL> = concat.parse().unwrap();
            let rule_name = format!("expand E {}", i);
            let rule = rewrite!(rule_name; "(!NONTERMINAL_E)" => concat_pattern);
            rules.push(rule)
        }

        rules
    }

    /// Generates a set of all possible substrings from self.idg.
    /// Each edge represents a set of equivalent positions, so just take one per edge.
    /// Then, build all 2-permutations of said positions.
    fn get_substr_set(&self) -> Vec<[(&PMatch, BlinkFillDSL); 2]> {
        let edges = self.idg.dag.raw_edges();
        let mut matches: Vec<(&PMatch, BlinkFillDSL)> = Vec::new();
        for e in edges {
            let mch = e.weight.iter().next().unwrap(); // get random item
            matches.push((mch, BlinkFillDSL::Start)); // one for start
            matches.push((mch, BlinkFillDSL::End)); // one for
        }
        let perms = matches.iter().permutations(2);
        perms.map(|p| [p[0].clone(), p[1].clone()]).collect()
    }

    /// Filters a set of substrings based on the followig criteria:
    /// A substring is useful if A.) it doesn't generate an empty string ever and
    /// B.) it doesn't have the same behavior as any other substring
    fn filter_substrs<'b>(
        &'b self,
        substr_set: Vec<[(&'b PMatch, BlinkFillDSL); 2]>,
    ) -> Vec<[(&PMatch, BlinkFillDSL); 2]> {
        let mut out: Vec<[(&PMatch, BlinkFillDSL); 2]> = vec![];
        let mut covered_strs: HashSet<String> = HashSet::new();

        for pos_pair in substr_set {
            let sub_expr: RecExpr<BlinkFillDSL> = format!(
                    "(!NONTERMINAL_SUBSTR !TERMINAL_INPUT (!TERMINAL_POS \"{}\" {} {}) (!TERMINAL_POS \"{}\" {} {})) ",
                    pos_pair[0].0.tau, pos_pair[0].0.k, pos_pair[0].1, pos_pair[1].0.tau, pos_pair[1].0.k, pos_pair[1].1).parse().unwrap();

            let intpr = DSLInterpreter::new(&sub_expr);
            let outputs = self
                .examples
                .iter()
                .map(|io| intpr.interpret(&io.0).unwrap())
                .collect_vec();

            if outputs.contains(&BlinkFillDSL::StrVal(String::from(""))) {
                continue; // substring produces empty string
            }

            let mut out_strs = String::from("");
            for output in outputs {
                // generate set of "predictions" on input
                match output {
                    BlinkFillDSL::StrVal(s) => out_strs += &(" ".to_owned() + &s),
                    _ => panic!("Expected StrVal."),
                }
            }

            if covered_strs.contains(&out_strs) {
                continue; // substring is already covered logic-wise
            } else {
                covered_strs.insert(out_strs);
                out.push(pos_pair); // substring is useful, add it
            }
        }
        out
    }
}

struct SynthesisCost<'a> {
    examples: &'a Vec<(String, String)>,
    egraph: &'a EGraph<BlinkFillDSL, ()>,
    early_break_on_fail: bool,
}
impl CostFunction<BlinkFillDSL> for SynthesisCost<'_> {
    type Cost = f64;
    /// Determines the cost of an enode. In our case, a program has 0 cost if
    /// it is correct over all I/O examples. Otherwise, if early_break_on_fail
    /// the cost of an incorrect program is large and fixed, else cost will
    /// accumulate for each incorrect example (but may take longer).
    fn cost<C>(&mut self, enode: &BlinkFillDSL, mut _costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        // if enode is not CONCAT, the current enode is not a complete expr
        if enode.to_string() != "!NONTERMINAL_CONCAT" {
            if enode.to_string() == "!NONTERMINAL_E" {
                return f64::INFINITY; // invalid program
            } else {
                return 0.0; // part of a valid program
            }
        }

        // build RecExpr from enode
        let get_first_enode = |id| self.egraph[id].nodes[0].clone();
        let expr: RecExpr<BlinkFillDSL> = enode.build_recexpr(get_first_enode);

        // run program over all inputs to build cost
        let mut cost = 0.0;
        let intpr = DSLInterpreter::new(&expr);
        for (input, output) in self.examples {
            match intpr.interpret(input) {
                Some(o) => {
                    if o.to_string() != *output {
                        cost += 10_000_000.0; // program is not correct on this example
                        if self.early_break_on_fail {
                            // break on incorrect if specified
                            break;
                        }
                    }
                }
                None => panic!("Synthesized program evaluated to None."),
            }
        }

        cost
    }
}
