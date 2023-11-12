use egg::*;
use grablink::dsl::*;

#[test]
fn test_concat_toplevel_concatenates() {
    let expr: RecExpr<BlinkFillDSL> = "(!NONTERMINAL_CONCAT wow you're really reading this)"
        .parse()
        .unwrap();
    let intpr = DSLInterpreter::new(&expr);
    assert_eq!(
        intpr.interpret(&String::from("")).unwrap().to_string(),
        "wowyou'rereallyreadingthis"
    )
}

#[test]
fn test_substr_gets_correct_indices() {
    let expr: RecExpr<BlinkFillDSL> = r#"
        (!NONTERMINAL_SUBSTR "abc1def1ghi1jkl1mno"
            (!TERMINAL_POS "\d+" 1 !TERMINAL_START) 
            (!TERMINAL_POS "\d+" -3 !TERMINAL_END))"#
        .parse()
        .unwrap();
    let intpr = DSLInterpreter::new(&expr);
    assert_eq!(
        intpr.interpret(&String::from("")).unwrap().to_string(),
        "1ghi1"
    )
}

#[test]
fn test_input_outputs_input() {
    let expr: RecExpr<BlinkFillDSL> = "(!NONTERMINAL_INPUT)".parse().unwrap();
    let intpr = DSLInterpreter::new(&expr);
    assert_eq!(
        intpr
            .interpret(&String::from("shadow wizard money gang"))
            .unwrap()
            .to_string(),
        "shadow wizard money gang"
    )
}

#[test]
fn test_pos_holds_children() {
    let expr: RecExpr<BlinkFillDSL> = "(!TERMINAL_POS \"regex\" 1 !TERMINAL_START)"
        .parse()
        .unwrap();
    let intpr = DSLInterpreter::new(&expr);
    let res = intpr.interpret(&String::from("")).unwrap();
    let children = res.children();

    println!("{}", children[0].to_string());

    assert_eq!(res.to_string(), "!TERMINAL_POS");
    assert_eq!(expr[children[0]].to_string(), "regex");
    assert_eq!(expr[children[1]].to_string(), "1");
    assert_eq!(expr[children[2]].to_string(), "!TERMINAL_START");
}

#[test]
fn test_lhs_nonterminals_are_none() {
    let exprs: Vec<RecExpr<BlinkFillDSL>> = vec![
        "!NONTERMINAL_E",
        "!NONTERMINAL_F",
        "!NONTERMINAL_P",
        "!NONTERMINAL_DIR",
    ]
    .iter()
    .map(|e| e.parse().unwrap())
    .collect();

    for expr in exprs {
        let intpr = DSLInterpreter::new(&expr);
        let res = intpr.interpret(&String::from(""));
        assert_eq!(res, None);
    }
}

#[test]
fn test_terminals_are_themselves() {
    let exprs: Vec<RecExpr<BlinkFillDSL>> = vec!["!TERMINAL_START", "!TERMINAL_END", "52", "wow"]
        .iter()
        .map(|e| e.parse().unwrap())
        .collect();

    for expr in exprs {
        let intpr = DSLInterpreter::new(&expr);
        let res = intpr.interpret(&String::from(""));

        assert_eq!(res.unwrap().to_string(), expr.to_string());
    }
}
