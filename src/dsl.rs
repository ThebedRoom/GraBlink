use crate::inputdatagraph::TOKENS;
use egg::*;
use once_cell::sync::Lazy;
use regex::{Match, Regex};

static STARTT: Lazy<Regex> = Lazy::new(|| Regex::new("^").unwrap());
static ENDT: Lazy<Regex> = Lazy::new(|| Regex::new("$").unwrap());

define_language! {
    pub enum BlinkFillDSL {
        "!NONTERMINAL_CONCAT" = Concat(Box<[Id]>),

        "!NONTERMINAL_SUBSTR" = Substr([Id; 3]), // substr of (string, pos, pos)
        "!TERMINAL_INPUT" = Input, // vi, current string input
        // ConstantStr(String), ???

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
                let mut failure = false;

                for val in vals {
                    match val {
                        Some(BlinkFillDSL::StrVal(s)) => acc += &s,
                        _ => failure = true,
                    }
                }

                if failure {
                    None
                } else {
                    Some(BlinkFillDSL::StrVal(acc))
                }
            }

            // substr evaluates all children and does str[start..end+1]
            BlinkFillDSL::Substr(children) => {
                let [_, pos_id1, pos_id2] = children;
                let vi = Some(input);

                let start_idx = match &self.program[*pos_id1] {
                    BlinkFillDSL::ConstantPos(i) => Some(*i as usize),
                    BlinkFillDSL::Pos(_) => {
                        let (r1, k1, dir1) = self.extract_pos(&self.program[*pos_id1]);
                        println!("{}", r1.unwrap().as_str());
                        match (vi, r1, k1, dir1) {
                            (Some(v), Some(r), Some(k), Some(d)) => {
                                Some(self.get_regex_index(v, &r, &k, d))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                let end_idx = match &self.program[*pos_id2] {
                    BlinkFillDSL::ConstantPos(i) => Some(*i as usize),
                    BlinkFillDSL::Pos(_) => {
                        let (r2, k2, dir2) = self.extract_pos(&self.program[*pos_id2]);
                        match (vi, r2, k2, dir2) {
                            (Some(v), Some(r), Some(k), Some(d)) => {
                                Some(self.get_regex_index(v, &r, &k, d))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                match (vi, start_idx, end_idx) {
                    (Some(v), Some(si), Some(ei)) => {
                        println!("{},{},{} => [{}]", v, si, ei, v[si..ei + 1].to_owned());
                        Some(BlinkFillDSL::StrVal(v[si..ei + 1].to_owned()))
                    }
                    _ => None,
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

    fn extract_strval<'a>(&'a self, expr: &'a BlinkFillDSL) -> Option<&String> {
        if let BlinkFillDSL::StrVal(s) = expr {
            Some(s)
        } else {
            None
        }
    }

    fn extract_constantpos<'a>(&'a self, expr: &'a BlinkFillDSL) -> Option<&i32> {
        if let BlinkFillDSL::ConstantPos(i) = expr {
            Some(i)
        } else {
            None
        }
    }

    fn extract_pos<'a>(
        &'a self,
        expr: &'a BlinkFillDSL,
    ) -> (Option<&Regex>, Option<i32>, Option<&'a BlinkFillDSL>) {
        match expr {
            BlinkFillDSL::Pos(children) => {
                let [r_id, k_id, dir_id] = children;
                let regex = self.extract_strval(&self.program[*r_id]);
                let k = self.extract_constantpos(&self.program[*k_id]);
                let dir = match &self.program[*dir_id] {
                    BlinkFillDSL::Start => Some(&self.program[*dir_id]),
                    BlinkFillDSL::End => Some(&self.program[*dir_id]),
                    _ => None,
                };

                if regex.unwrap_or(&String::new()) == "StartT" {
                    (Some(&STARTT), Some(1), Some(&BlinkFillDSL::Start))
                } else if regex.unwrap_or(&String::new()) == "EndT" {
                    (Some(&ENDT), Some(1), Some(&BlinkFillDSL::End))
                } else {
                    match regex {
                        Some(r) => (Some(TOKENS.get(r.as_str()).unwrap()), k.copied(), dir),
                        _ => (None, k.copied(), dir),
                    }
                }
            }
            _ => (None, None, None),
        }
    }

    fn get_regex_index(&self, vi: &String, re: &Regex, k: &i32, dir: &BlinkFillDSL) -> usize {
        let matches: Vec<Match> = re.find_iter(vi).collect();
        let idx = if *k >= 0 {
            *k - 1
        } else {
            (matches.len() as i32) + *k
        };

        let chosen_match = matches.iter().nth(idx as usize);
        match dir {
            BlinkFillDSL::Start => chosen_match.unwrap().start(),
            BlinkFillDSL::End => chosen_match.unwrap().end() - 1,

            _ => panic!("Expected Dir"),
        }
    }
}
