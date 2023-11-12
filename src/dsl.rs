use egg::*;
use regex::{Match, Regex};

define_language! {
    pub enum BlinkFillDSL {
        "!NONTERMINAL_E" = E, // E -> concat(f1, f2, ...)
        "!NONTERMINAL_CONCAT" = Concat(Box<[Id]>),

        "!NONTERMINAL_F" = F, // F -> substr(input, p, p) | constr(Symbol)
        "!NONTERMINAL_SUBSTR" = Substr([Id; 3]), // substr of (string, pos, pos)
        "!NONTERMINAL_INPUT" = Input, // vi, current string input
        // ConstantStr(String), ???

        "!NONTERMINAL_P" = P, // p -> pos(token, Num, Dir) | constpos(int)
        "!TERMINAL_POS" = Pos([Id; 3]), // pos of (token, k, dir)

        "!NONTERMINAL_DIR" = Dir, // Dir -> Start | End
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
            // lhs nonterminals cannot be evaluated, so they are None
            BlinkFillDSL::E => None,
            BlinkFillDSL::F => None,
            BlinkFillDSL::P => None,
            BlinkFillDSL::Dir => None,

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
                let [vi_id, pos_id1, pos_id2] = children;
                let vi = self.extract_strval(&self.program[*vi_id]);

                let start_idx = match &self.program[*pos_id1] {
                    BlinkFillDSL::ConstantPos(i) => Some(*i as usize),
                    BlinkFillDSL::Pos(_) => {
                        let (r1, k1, dir1) = self.extract_pos(&self.program[*pos_id1]);
                        match (vi, r1, k1, dir1) {
                            (Some(v), Some(r), Some(k), Some(d)) => {
                                Some(self.get_regex_index(v, &r, k, d))
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
                                Some(self.get_regex_index(v, &r, k, d))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                match (vi, start_idx, end_idx) {
                    (Some(v), Some(si), Some(ei)) => {
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
    ) -> (Option<Regex>, Option<&i32>, Option<&'a BlinkFillDSL>) {
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

                match regex {
                    Some(r) => (Some(Regex::new(r).unwrap()), k, dir),
                    _ => (None, k, dir),
                }
            }
            _ => (None, None, None),
        }
    }

    fn get_regex_index(&self, vi: &String, re: &Regex, k: &i32, dir: &BlinkFillDSL) -> usize {
        let matches: Vec<Match> = re.find_iter(vi).collect();
        let idx = if *k >= 0 {
            *k
        } else {
            (matches.len() as i32) + *k
        };
        match dir {
            BlinkFillDSL::Start => {
                let chosen_match = matches.iter().nth(idx as usize);
                chosen_match.unwrap().start()
            }
            BlinkFillDSL::End => {
                let mut rmatches = matches.iter().rev();
                let chosen_match = rmatches.nth(idx as usize);
                chosen_match.unwrap().start()
            }

            _ => panic!("Expected Dir"),
        }
    }
}
