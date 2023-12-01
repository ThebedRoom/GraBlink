use crate::dsl::{BlinkFillDSL, DSLInterpreter};
use crate::inputdatagraph::{InputDataGraph, PMatch};
use egg::*;
use itertools::Itertools;
use log::debug;
use rayon::prelude::*;
use std::collections::HashSet;

type Substr<'a> = [(&'a PMatch, BlinkFillDSL); 2];
#[allow(dead_code)]
#[derive(Debug, Clone)]
enum ConcatOperand<'a> {
    ConstantStr(String),
    Substr(Substr<'a>),
    Plus(Box<ConcatOperand<'a>>, Box<ConcatOperand<'a>>),
    Sub(Box<ConcatOperand<'a>>, Box<ConcatOperand<'a>>),
    Mul(Box<ConcatOperand<'a>>, Box<ConcatOperand<'a>>),
    Div(Box<ConcatOperand<'a>>, Box<ConcatOperand<'a>>),
}

impl ConcatOperand<'_> {
    pub fn to_string(&self) -> String {
        match self {
            ConcatOperand::ConstantStr(s) => format!("(\"{}\") ", s),
            ConcatOperand::Substr(s) => format!(
                 "(!NONTERMINAL_SUBSTR !TERMINAL_INPUT (!TERMINAL_POS \"{}\" {} {}) (!TERMINAL_POS \"{}\" {} {})) ",
                s[0].0.tau, s[0].0.k, s[0].1, s[1].0.tau, s[1].0.k, s[1].1),
            ConcatOperand::Plus(a, b) => format!("(!NONTERMINAL_PLUS {} {}) ", a.to_string(), b.to_string()),
            ConcatOperand::Sub(a, b) => format!("(!NONTERMINAL_SUB {} {}) ", a.to_string(), b.to_string()),
            ConcatOperand::Mul(a, b) => format!("(!NONTERMINAL_MUL {} {}) ", a.to_string(), b.to_string()),
            ConcatOperand::Div(a, b) => format!("(!NONTERMINAL_DIV {} {}) ", a.to_string(), b.to_string()),
        }
    }
}

pub struct Synthesizer<'a> {
    examples: &'a Vec<(String, String)>,
    idg: &'a InputDataGraph<HashSet<PMatch>>,
    odg: &'a InputDataGraph<HashSet<PMatch>>,
}

impl<'a> Synthesizer<'a> {
    pub fn new(
        examples: &'a Vec<(String, String)>,
        idg: &'a InputDataGraph<HashSet<PMatch>>,
        odg: &'a InputDataGraph<HashSet<PMatch>>,
    ) -> Synthesizer<'a> {
        Synthesizer { examples, idg, odg }
    }

    /// Given some input string `s`, synthesize a program that works
    /// over all self.examples, based on information from self.idg and self.odg.
    pub fn synthesize(&self, s: &str) -> Option<(RecExpr<BlinkFillDSL>, f64)> {
        let expr: RecExpr<BlinkFillDSL> = s.parse().unwrap();
        // max concat arity needed is min length of output
        let max_concat_arity = self.examples.iter().map(|e| e.1.len()).min().unwrap();

        for arity in 1..max_concat_arity + 1 {
            // Generate EGraph and run rewrite rules
            let rules = self.make_rules(arity);
            if rules.is_empty() {
                debug!(
                    "No correct expressions of arity {} found, moving to next arity...",
                    arity
                );
                continue;
            }
            debug!(
                "Generated {} correct expressions of arity {}, building & extracting best...",
                rules.len(),
                arity
            );
            let runner = Runner::default().with_expr(&expr).run(&rules);
            let extractor = Extractor::new(&runner.egraph, SynthesisCost);
            let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);
            return Some((best_expr, best_cost));
        }

        None // no solution found
    }

    /// Given a specific concat_arity, build the rewrite rules
    /// for all correct concat expressions of said arity.
    fn make_rules(&self, concat_arity: usize) -> Vec<Rewrite<BlinkFillDSL, ()>> {
        // get all useful substrings, constant strings, and possible numeric ops on top of those
        let substrs = self.get_substr_set();
        let useful_substrs: Vec<ConcatOperand> = self.filter_substrs(&substrs);
        let cstrs = self.get_conststr_set();
        let num_ops = self.get_num_set(&useful_substrs);

        let all_operands: Vec<&ConcatOperand> = vec![&useful_substrs, &cstrs, &num_ops]
            .into_iter()
            .flat_map(|set| set.iter())
            .collect();

        // get all permutations of length `concat_arity` of concat operands
        let all_orderings = all_operands.iter().permutations(concat_arity).collect_vec();
        debug!(
            "Arity {} has {} possible orderings, making rules...",
            concat_arity,
            all_orderings.len()
        );

        // for each ordering, generate the corresponding expression and add a rule if it is correct
        let rules: Vec<_> = (0..all_orderings.len())
            .into_par_iter()
            .map(|i| {
                let ordering = all_orderings[i].clone();

                let mut concat = String::from("(!NONTERMINAL_CONCAT ");
                for operand in ordering {
                    concat += &operand.to_string();
                }
                concat += ")";

                let expr: RecExpr<BlinkFillDSL> = concat.parse().unwrap();
                let intpr = DSLInterpreter::new(&expr);
                let mut correct = true;

                for (input, output) in self.examples {
                    if let Some(BlinkFillDSL::StrVal(s)) = intpr.interpret(input) {
                        if s != *output {
                            correct = false;
                            break;
                        }
                    } else {
                        correct = false;
                        break;
                    }
                }

                if correct {
                    let concat_pattern: Pattern<BlinkFillDSL> = concat.parse().unwrap();
                    let rule_name = format!("expand E {}", i);
                    let rule = rewrite!(rule_name; "(!NONTERMINAL_E)" => concat_pattern);
                    Some(rule)
                } else {
                    None
                }
            })
            .collect();

        rules.iter().flatten().map(|e| e.clone()).collect() // Filter None's
    }

    /// Generates a set of all possible substrings from self.idg.
    /// Each edge represents a set of equivalent positions, so just take one per edge.
    /// Then, build all 2-permutations of said positions.
    fn get_substr_set(&self) -> Vec<Substr> {
        let edges = self.idg.dag.raw_edges();
        let mut matches: Vec<(&PMatch, BlinkFillDSL)> = Vec::new();
        for e in edges {
            let mch = e.weight.iter().next().unwrap(); // get random item
            matches.push((mch, BlinkFillDSL::Start)); // one for start
            matches.push((mch, BlinkFillDSL::End)); // one for end
        }
        let perms = matches.iter().permutations(2);
        perms.map(|p| [p[0].clone(), p[1].clone()]).collect()
    }

    /// Filters a set of substrings based on the followig criteria:
    /// A substring is useful if A.) it doesn't generate an empty string ever and
    /// B.) it doesn't have the same behavior as any other substring
    fn filter_substrs<'b>(
        &'b self,
        substr_set: &Vec<[(&'b PMatch, BlinkFillDSL); 2]>,
    ) -> Vec<ConcatOperand> {
        let mut out: Vec<Substr> = vec![];
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
                out.push(pos_pair.clone()); // substring is useful, add it
            }
        }
        out.iter()
            .map(|s| ConcatOperand::Substr(s.clone()))
            .collect()
    }

    fn get_conststr_set(&self) -> Vec<ConcatOperand> {
        self.odg
            .dag
            .raw_edges()
            .iter()
            .map(|x| x.weight.clone())
            .flatten()
            .filter(|x| x.constantstr)
            .map(|k| String::from(k.tau))
            .unique()
            .map(|s| ConcatOperand::ConstantStr(String::from(s.clone())))
            .collect()
    }

    /// Generates all possible binary op exprs (2perms) from a set of operands
    fn get_num_set(&'a self, operands: &'a Vec<ConcatOperand>) -> Vec<ConcatOperand> {
        let mut exprs: Vec<ConcatOperand> = vec![];
        let perms = operands.iter().permutations(2);
        for perm in perms {
            let (l, r) = (Box::new(perm[0].clone()), Box::new(perm[1].clone()));
            exprs.push(ConcatOperand::Plus(l.clone(), r.clone()));
            exprs.push(ConcatOperand::Sub(l.clone(), r.clone()));
            exprs.push(ConcatOperand::Mul(l.clone(), r.clone()));
            exprs.push(ConcatOperand::Div(l.clone(), r.clone()));
        }
        exprs
    }
}

struct SynthesisCost;
impl CostFunction<BlinkFillDSL> for SynthesisCost {
    type Cost = f64;
    fn cost<C>(&mut self, enode: &BlinkFillDSL, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if enode.to_string() == "!NONTERMINAL_E" {
            return f64::INFINITY; // Start symbol, invalid program
        }
        let cost = match enode {
            BlinkFillDSL::Substr(_) => 1.0, // ConstantStr > SubStr
            _ => 0.0,
        };

        enode.fold(cost, |sum, id| sum + costs(id))
    }
}
