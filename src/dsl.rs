use crate::inputdatagraph::TOKENS;
use egg::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::{Match, Regex};

static STARTT: Lazy<Regex> = Lazy::new(|| Regex::new("^").unwrap());
static ENDT: Lazy<Regex> = Lazy::new(|| Regex::new("$").unwrap());

define_language! {
    pub enum BlinkFillDSL {
        "!NONTERMINAL_CONCAT" = Concat(Box<[Id]>),
        "!NONTERMINAL_SUBSTR" = Substr([Id; 3]), // substr of (string, pos, pos)
        "!TERMINAL_INPUT" = Input, // vi, current string input
        "!TERMINAL_POS" = Pos([Id; 3]), // pos of (token, k, dir)
        "!TERMINAL_START" = Start,
        "!TERMINAL_END" = End,
        ConstantPos(i32),
        StrVal(String),
    }
}

pub struct DSLInterpreter<'a> {
    program: &'a RecExpr<BlinkFillDSL>,
}

impl DSLInterpreter<'_> {
    pub fn new(program: &RecExpr<BlinkFillDSL>) -> DSLInterpreter {
        DSLInterpreter { program }
    }

    pub fn interpret(&self, input: &String) -> Option<BlinkFillDSL> {
        let root = self.program.as_ref().last().unwrap(); // grab root expr
        self.eval(root, input)
    }

    fn eval(&self, expr: &BlinkFillDSL, input: &String) -> Option<BlinkFillDSL> {
        match expr {
            // concat evaluates all children then concatenates them
            BlinkFillDSL::Concat(_) => {
                let vals = expr
                    .children()
                    .iter()
                    .map(|id| self.eval(&self.program[*id], &input));

                let mut acc = String::from("");
                for val in vals {
                    // concat all vals into one string
                    match val {
                        Some(BlinkFillDSL::StrVal(s)) => acc += &s,
                        _ => return None, // encountered bad value
                    }
                }
                Some(BlinkFillDSL::StrVal(acc))
            }

            // substr evaluates all children and does str[start..end+1]
            BlinkFillDSL::Substr(children) => {
                let [_, pos_id1, pos_id2] = children;
                let start = self.get_index_from_pos_id(pos_id1, input);
                let end = self.get_index_from_pos_id(pos_id2, input);
                if end > start {
                    Some(BlinkFillDSL::StrVal(input[start..end].to_owned()))
                } else {
                    Some(BlinkFillDSL::StrVal(String::from("")))
                }
            }

            // Input simply substitutes the input
            BlinkFillDSL::Input => Some(BlinkFillDSL::StrVal(input.to_string())),

            // All terminals simply evaluate to themselves
            BlinkFillDSL::Pos(children) => Some(BlinkFillDSL::Pos(*children)),
            BlinkFillDSL::Start => Some(BlinkFillDSL::Start),
            BlinkFillDSL::End => Some(BlinkFillDSL::End),
            BlinkFillDSL::ConstantPos(k) => Some(BlinkFillDSL::ConstantPos(*k)),
            BlinkFillDSL::StrVal(s) => Some(BlinkFillDSL::StrVal(s.to_string())),
        }
    }

    fn get_index_from_pos_id<'a>(&'a self, pos_id: &Id, input: &String) -> usize {
        if let BlinkFillDSL::ConstantPos(p) = &self.program[*pos_id] {
            return *p as usize;
        }
        let (re, k, dir) = self.extract_pos_from_id(pos_id);

        if re.as_str() == "$" {
            return input.len() - 1; // end of str would be out of bounds if not checked
        }

        let matches: Vec<Match> = re.find_iter(input).collect();
        let match_idx = if k >= 0 {
            k - 1 // k is indexed at 1, vec is indexed at 0
        } else {
            (matches.len() as i32) + k // negative k means k + len
        };
        // println!("{:?} {}", matches, matches.len());
        // println!("{} {} {} {} {}", re, k, match_idx, dir, input);
        let chosen_match = matches.iter().nth(match_idx as usize).unwrap();
        match dir {
            BlinkFillDSL::Start => chosen_match.start(),
            BlinkFillDSL::End => chosen_match.end(),
            _ => panic!("Expected Dir"),
        }
    }

    fn extract_pos_from_id<'a>(&'a self, pos_id: &Id) -> (Regex, i32, &'a BlinkFillDSL) {
        match &self.program[*pos_id] {
            BlinkFillDSL::Pos(children) => {
                let [c1, c2, c3] = children.map(|id| &self.program[id]);
                match (c1, c2, c3) {
                    (BlinkFillDSL::StrVal(r), BlinkFillDSL::ConstantPos(k), dir) => {
                        let regex = match TOKENS.get(r.as_str()) {
                            Some(res) => res.clone(),
                            None => {
                                if r == "StartT" {
                                    STARTT.clone()
                                } else if r == "EndT" {
                                    ENDT.clone()
                                } else {
                                    Regex::new(&regex::escape(r)).unwrap()
                                }
                            }
                        };
                        (regex, *k, dir)
                    }
                    (_, _, _) => panic!("Bad position."),
                }
            }
            _ => panic!("Expected ConstantPos or Pos."),
        }
    }
}
