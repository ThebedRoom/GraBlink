use crate::dsl::{BlinkFillDSL, DSLInterpreter};
use crate::inputdatagraph::{InputDataGraph, PMatch};
use egg::*;
use itertools::Itertools;
use std::collections::HashSet;

pub struct Synthesizer<'a> {
    examples: Vec<(String, String)>,
    idg: &'a InputDataGraph<HashSet<PMatch>>,
    max_concat_arity: usize,
    iter_limit: usize,
}

impl<'a> Synthesizer<'a> {
    pub fn new(
        examples: Vec<(String, String)>,
        idg: &InputDataGraph<HashSet<PMatch>>,
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

        // get set of all 2perms of pmatches, this gives all possible substrs
        let substrs = self.get_substr_set();

        println!("{}", substrs.len());
        for perm in substrs {
            println!("HERE: {} {}", perm[0].0, perm[0].1.to_string());
            println!("HERE: {} {}", perm[1].0, perm[1].1.to_string());
        }
        // given N, generate all Nperms of substrs & conststrs

        rules
    }

    fn get_substr_set(&self) -> Vec<Vec<(&PMatch, BlinkFillDSL)>> {
        let edges = self.idg.dag.raw_edges();
        let mut matches: Vec<(&PMatch, BlinkFillDSL)> = Vec::new();
        for e in edges {
            let mch = e.weight.iter().next().unwrap(); // get random item
            matches.push((mch, BlinkFillDSL::Start));
            matches.push((mch, BlinkFillDSL::End));
        }
        let perms = matches.iter().permutations(2);

        perms.map(|p| vec![p[0].clone(), p[1].clone()]).collect()
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

        cost
    }
}
