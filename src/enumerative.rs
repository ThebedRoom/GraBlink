use std::collections::HashSet;
use itertools::Itertools;

use crate::{vsa::{Program, Position, Number, IOPair, Dir, regex_match}, inputdatagraph::{InputDataGraph, PMatch}};

pub fn sub_s(pm: HashSet<PMatch>, bound: usize) -> Vec<Program>{
    let mut temp: HashSet<Position> = pm.iter().map(|pm| {
        Position::RegexPos(pm.tau.clone(), pm.k, Dir::Start)
    }).collect();
    temp.extend(pm.iter().map(|pm| {
        Position::RegexPos(pm.tau.clone(), pm.k, Dir::End)
    }));
    temp.extend((0..bound).map(|pm| Position::ConstantPos(pm)));

    let mut v = vec![];

    for i in temp.iter() {
        for j in temp.iter() {
            match(i,j){
                (Position::ConstantPos(uno), Position::ConstantPos(dos)) => {
                    if uno < dos {
                        v.push(Program::SubStr(i.clone(), j.clone()));
                    }
                },
                _ => {v.push(Program::SubStr(i.clone(), j.clone()));}
            }
        }
    }
    v
}

fn evaluate(prog: &Program, input: &IOPair) -> Option<String> {
    match prog {
        Program::ConstantStr(s) => Some(s.clone()),
        Program::SubStr(p1, p2) => {
            let start = match p1 {
                Position::ConstantPos(p) => Some(p.clone()),
                Position::RegexPos(tok, k, pos) => {
                    let var = regex_match(tok, *k, &input.input);

                    match var {
                        Some((x,y)) => Some(if *pos == Dir::Start {x} else {y}),
                        None => None,
                    }
                },
                
            };
            let end = match p2 {
                Position::ConstantPos(p) => Some(p.clone()),
                Position::RegexPos(tok, k, pos) => {
                    let var = regex_match(tok, *k, &input.input);

                    match var {
                        Some((x,y)) => Some(if *pos == Dir::Start {x} else {y}),
                        None => None,
                    }
                },
            };
            match (start, end) {
                (Some(s), Some(e)) => match input.input.get(s.clone()..e.clone()) {
                    Some(s) => Some(String::from(s)),
                    None => None,
                },
                _ => None
            }
            
        },
        Program::Concat(v) => {
            let mut out = String::new();
            for s in v {
                match evaluate(s, input) {
                    Some(t) => { out.push_str(t.as_str()); }
                    None => { return None; }
                }
            }
            Some(out)
        },
        Program::NumToStr(number) => {
            match number.evaluate(input) {
                Some(n) => Some(n.to_string()),
                None => None
            }
        }
    }
}

fn verify(prog: &Program, io: &Vec<IOPair>) -> bool {
    for pair in io.iter() {
        if evaluate(prog,pair) != Some(pair.output.clone()) {
            return false;
        }
    }
    true
}

pub fn enumerative(input: &'static Vec<String>, idg: &InputDataGraph<HashSet<PMatch>>, odg: &InputDataGraph<HashSet<PMatch>>) -> Option<Program>{
    let is: Vec<IOPair> = input.chunks(2)
        .map(|x| IOPair::new(x[0].clone(),x[1].clone(), false))
        .collect();

    let pmatches: HashSet<PMatch> = idg.dag.raw_edges().iter().map(
        |x| {
            x.weight.clone()
        }
    )
    .flatten()
    .filter(|x| x.tau != "StartT" && x.tau != "EndT" && !x.constantstr)
    .collect();    

    let cmatches: HashSet<Program> = odg.dag.raw_edges().iter().map(
        |x| {
            x.weight.clone()
        }
    )
    .flatten()
    .filter(|x| x.constantstr)
    .map(|k | {
        match k.tau.parse() {
            Ok(numer) => Program::NumToStr(Number::ConstantNum(numer)),
            Err(_) => Program::ConstantStr(k.tau),
        }
    })
    .collect();    


    let var = is.iter().map(|x| x.output.len()).min().unwrap_or(0);

    let mut substring = sub_s(pmatches,var);
    substring.extend(cmatches);

    //let mut counter = 0;
    for i in 1..var+1 {
        for var in substring.iter().permutations(i).unique().into_iter(){
            let p = Program::Concat(var.iter().map(|&v| v.clone()).collect());
            //counter += 1;
            //if bob == None && verify(&p,&is){
            if verify(&p, &is){
                //println!("Times ran {}",counter);
                return Some(p);
                //bob = Some(p);
            }
        }
    }
    //println!("The second counter: {}",counter);
    //bo
    None
}
