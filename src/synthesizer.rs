use crate::dsl::*;
use egg::*;

pub struct Synthesizer {
    examples: Vec<String>,
    max_concat_arity: usize,
    iter_limit: usize,
}

impl Synthesizer {
    pub fn new(examples: Vec<String>, max_concat_arity: usize, iter_limit: usize) -> Synthesizer {
        Synthesizer {
            examples,
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

        // Create an extractor with our custom cost function
        let extractor = Extractor::new(&runner.egraph, SynthesisCost);
        // Extract lowest cost expression
        let (best_cost, best) = extractor.find_best(runner.roots[0]);
        println!("{} expanded to {} with cost {}", expr, best, best_cost);
        best // return best expr
    }

    fn make_rules(&self) -> Vec<Rewrite<BlinkFillDSL, ()>> {
        let mut rules: Vec<Rewrite<BlinkFillDSL, ()>> = Vec::new();

        // dynamic concat rules
        rules.extend((1..self.max_concat_arity).map(|n| {
            let rule = format!("(concat {})", "F ".repeat(n))
                .parse::<Pattern<BlinkFillDSL>>()
                .unwrap();
            rewrite!(format!("concat {}", n); "(E)" => rule)
        }));

        // static rules
        rules.extend(vec![
            rewrite!("F -> substr(vi, pl, pr)"; "(F)" => "(substr input p p)"),
            rewrite!("F -> ConstantStr(s)"; "(F)" => "(constr)"),
            rewrite!("p -> pos(token, k, dir)"; "(p)" => "(pos match dir)"),
            rewrite!("p -> ConstantPos(k)"; "(p)" => "(constpos)"),
            rewrite!("Dir -> Start"; "(dir)" => "(start)"),
            rewrite!("Dir -> End"; "(dir)" => "(end)"),
        ]);

        rules
    }
}

struct SynthesisCost;
impl CostFunction<BlinkFillDSL> for SynthesisCost {
    type Cost = f64;
    fn cost<C>(&mut self, enode: &BlinkFillDSL, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            // For now, just make the largest expression w/o non-terms
            // in reality, we will define cost based on
            //  1. Complete, no non-terms
            //  2. Correct, io examples work
            BlinkFillDSL::E => 100.0,
            BlinkFillDSL::F => 100.0,
            BlinkFillDSL::P => 100.0,
            BlinkFillDSL::Dir => 100.0,
            _ => -1.0,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
