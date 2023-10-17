use peg;
use std::{
    collections::HashMap,
    fmt,
    ops::{Deref, DerefMut},
};

peg::parser! {
    grammar expr_parser() for str {
        rule whitespace() = [' ' | '\t' | '\n' | '\r']*

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule list() -> Expr
            = "(" l:Expr() ** whitespace() ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = symbol() / list()
    }
}

fn parse(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Sym(String),
    List(Vec<Expr>),
}

impl Deref for Expr {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Self::Target {
        match self {
            Expr::List(vec) => vec,
            e => panic!("Can only deref Expr::List. ex:{e:?}"),
        }
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Expr::List(vec) => vec,
            e => panic!("Can only deref Expr::List. ex:{e:?}"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::List(lst) => {
                let str_list: Vec<String> = lst.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", str_list.join(" "))
            }
        }
    }
}

fn sym(s: &str) -> Expr {
    Expr::Sym(s.to_string())
}

fn list(strs: Vec<&str>) -> Expr {
    Expr::List(strs.iter().map(|s| sym(s)).collect::<Vec<_>>())
}

fn liste(es: Vec<Expr>) -> Expr {
    Expr::List(es)
}
// head (f x) -> f, head(f) -> Sym
fn head(e: Expr) -> Expr {
    match e {
        Expr::Sym(_) => sym("Sym"),
        Expr::List(es) => es[0].clone(),
    }
}

fn named_rebuild_all(expr: Expr, map: &HashMap<Expr, Expr>) -> Expr {
    // First, check if the entire expression exists in the map and replace it if it does
    if let Some(replacement) = map.get(&expr) {
        return replacement.clone();
    }

    // If the expression is not in the map, proceed with the recursion
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(list) => {
            // Recursively rebuild all sub-expressions in the list
            let new_list: Vec<Expr> = list
                .into_iter()
                .map(|e| named_rebuild_all(e, map))
                .collect();
            Expr::List(new_list)
        }
    }
}

fn pos_map_rebuild(pos: Vec<usize>, pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    if let Some(replacement) = pos_map.get(&pos) {
        return replacement.clone();
    }

    match pat {
        Expr::Sym(_) => pat,
        Expr::List(es) => {
            let mut new_es = vec![];
            for (i, e) in es.iter().enumerate() {
                let mut new_pos = pos.clone();
                new_pos.push(i);
                let new_e = pos_map_rebuild(new_pos, e.clone(), pos_map);
                new_es.push(new_e);
            }
            Expr::List(new_es)
        }
    }
}

fn rebuild_all(pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    pos_map_rebuild(vec![], pat, pos_map)
}

fn splice_sequences(expr: Expr) -> Expr {
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(mut list) => {
            let mut i = 0;
            while i < list.len() {
                list[i] = splice_sequences(list[i].clone());
                i += 1;
            }

            let mut new_list = Vec::new();
            let mut i = 0;
            while i < list.len() {
                let item = list[i].clone();
                if let Expr::List(ref sublist) = item {
                    if let Some(Expr::Sym(head)) = sublist.first() {
                        if head == "sequence" {
                            new_list.extend_from_slice(&sublist[1..]);
                            i += 1;
                            continue;
                        }
                    }
                }
                new_list.push(item);
                i += 1;
            }
            Expr::List(new_list)
        }
    }
}

fn rebuild_and_splice(
    pat: Expr,
    // named_map: &HashMap<Expr, Expr>,
    pos: &Vec<usize>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    splice_sequences(pos_map_rebuild(pos.clone(), pat, pos_map))
}

// we assume that p has a blank object for a head
fn is_blank_match(e: Expr, p: Expr) -> bool {
    if let Expr::List(ps) = p {
        if ps.len() == 2 {
            let p_head = &ps[1];
            if p_head == &head(e) {
                true
            } else {
                false
            }
        } else {
            true
        }
    } else {
        panic!("is_blank_match needs a list for p")
    }
}

fn my_match(
    ex: Expr,
    pat: Expr,
    pos: &Vec<usize>,
    pos_map: &mut HashMap<Vec<usize>, Expr>,
) -> bool {
    println!("{pos:?} | {ex} | {pat} | {pos_map:?}");

    let pat_syms = vec![sym("blank"), sym("blank_seq")];
    match (ex.clone(), pat.clone()) {
        (Expr::Sym(e), Expr::Sym(p)) => ex == pat,
        (Expr::Sym(e), Expr::List(ps)) => {
            if pat_syms.contains(&ps[0]) {
                pos_map.insert(pos.clone(), ex);
                true
            } else {
                false
            }
        }
        (Expr::List(es), Expr::List(ps)) => {
            if pat_syms.contains(&ps[0]) {
                pos_map.insert(pos.clone(), ex);
                return true;
            }

            let mut new_pos = pos.clone();
            new_pos.push(0); // we are at the head
            if !my_match(es[0].clone(), ps[0].clone(), &new_pos, pos_map) {
                return false;
            }

            'outer: for (i, pi) in ps.iter().enumerate().skip(1) {
                let mut new_pos = pos.clone();
                new_pos.push(i); // we are at the head
                if head(pi.clone()) == sym("blank_seq") {
                    'inner: for j in 1..=es[1..].len() {
                        let mut elts = vec![sym("sequence")];
                        if i+j > es.len() {
                            break 'outer;
                        }
                        for seq_e in &es[i..i + j] {
                            elts.push(seq_e.clone());
                        }
                        let seq = liste(elts);
                        pos_map.insert(new_pos.clone(), seq.clone());
                        
                        let new_pat = rebuild_and_splice(pat.clone(), &pos, pos_map);
                        println!("new_pat in bs: {new_pat}");
                        if my_match(ex.clone(), new_pat, pos, pos_map) {
                            break 'outer;
                        } else {
                            // break 'outer;
                            // i think we need to revert pos_map to whatever it was before this my_match call
                        }
                    }
                } else {
                    if !my_match(es[i].clone(), ps[i].clone(), &new_pos, pos_map) {
                        break 'outer;
                    }
                }
            }

            let final_pat = rebuild_and_splice(pat.clone(), &pos, pos_map);
            println!("final comparison: POS: {pos:?} | PAT: {pat} | NEW_PAT: {final_pat} | EX: {ex} || {pos_map:?}");
            if final_pat == ex {
                return true;
            }
            false
        }
        _ => false,
    }
}

fn main() {
    let test_cases = vec![
        (sym("1"), sym("1"), true),         // goes to "1" == "1" Sym, Sym arm
        (sym("1"), parse("(blank)"), true), // Sym Sym arm with blank
        // (sym("1"), Expr::List(vec![sym("1")]), false), // Sym List -> false
        // (Expr::List(vec![sym("1")]), sym("1"), false), // List Sym
        // // (1) | (blank)
        // (Expr::List(vec![sym("1")]), blank.clone(), true), // List, sym, with blank
        // (lhs.clone(), rhs.clone(), false),                 // List, sym, with blank
        // // (lhs.clone(), list(vec!["f", "blank", "blank"]), true), // List, sym, with blank
        // (
        //     lhs.clone(),
        //     liste(vec![sym("f"), blank.clone(), blank.clone()]),
        //     true,
        // ), // List, sym, with blank
        // (sym("f"), list(vec!["blank", "Sym"]), true),
        // (sym("f"), list(vec!["blank", "f"]), false),
        // (list(vec!["f", "x"]), list(vec!["blank", "f"]), true),
        // (list(vec!["f", "x"]), list(vec!["blank", "g"]), false),
        // (parse("(f (a b))"), parse("(f (blank))"), true),
        // (parse("(f (a b))"), parse("(f (blank a))"), true),
        // (parse("(f x)"), parse("((blank) (blank))"), true),
        // (parse("f"), parse("(pattern x (blank))"), true),
        // (parse("(f)"), parse("(pattern x (blank))"), true),
        // (parse("(f x)"), parse("((pattern x (blank)) (blank))"), true),
        // (
        //     parse("(f a b c)"),
        //     parse("(f (pattern x (blank_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a b c)"),
        //     parse("(f (pattern x (blank_seq)) (pattern y (blank_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a a)"),
        //     parse("(f (pattern x (blank_seq)) (pattern x (blank_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a (g b))"),
        //     parse("(f (pattern x (blank_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a)"),
        //     parse("(f (pattern x (blank_null_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a)"),
        //     parse("(f (pattern x (blank_null_seq)) a)"),
        //     true,
        // ),
        // (
        //     parse("(f a b c a b)"),
        //     parse("(f (pattern x (blank_seq)) c (pattern x (blank_seq)))"),
        //     true,
        // ),

        // (
        //     parse("(f (a b) c a b)"),
        //     parse("(f (pattern x (blank b)) (pattern y (blank_seq)))"),
        //     false,
        // ),
        // (
        //     parse("(f (a b) c a b)"),
        //     parse("(f (pattern x (blank a)) (pattern y (blank_seq)))"),
        //     true,
        // ),
        // (
        //     parse("(f a b c d)"),
        //     parse("(f (blank_seq) (pattern y (blank_seq)))"),
        //     true,
        // ),
        // // fails todo fix blank_seq with head
        // (
        //     parse("(f (a b) (a c) (b d))"),
        //     parse("(f (pattern x (blank_seq a)))"),
        //     false,
        // ),
        // (
        //     parse("(f (a b) (a c) (b d))"),
        //     parse("(f (pattern x (blank_seq a)) (b d))"),
        //     true,
        // ),
        // // pos : Vec<usize> where are we in the pattern Expr
        // (
        //     parse("(f (a b) (a c) (b d))"),
        //     parse("(f (blank_seq a) (b d))"),
        //     true,
        // ),
        // (
        //     parse("(f (a b) (a c) (b d))"),
        //     parse("(f (blank_seq a))"),
        //     false,
        // ),
        
        (parse("(f a b c)"), parse("(f (blank_seq))"), true),
        (parse("(f a b c)"), parse("((blank) (blank_seq))"), true),
        
        (parse("((k a) b)"), parse("((k a) b)"), true),
        (parse("((k a) b)"), parse("((k (blank)) b)"), true),
        (parse("(f a b c)"), parse("(f (blank_seq) (blank) (blank_seq))"), true),
        
        (parse("(f a b c)"), parse("(f (blank_seq) (blank_seq))"), true),
        
        
        // (parse("((k a b b) b)"), parse("((k (blank_seq)) b)"), true),
        (parse("(f a b b)"), parse("(f (blank_seq))"), true),
        // (parse("(a a)"), parse("(a blank_seq)"), true),
    ];

    // list(vec!["f", "a", "b", "c"]), list(vec!["f", sym("blank_sequence")])
    for (i, (ex, pat, expected)) in test_cases.iter().enumerate() {
        println!("testing case {i}: {ex} | {pat} ");
        let pos = vec![];
        let mut pos_map = HashMap::new();
        // let mut named_map = HashMap::new();
        let m = my_match(ex.clone(), pat.clone(), &pos, &mut pos_map);
        let rebuilt_ex = rebuild_and_splice(pat.clone(), &vec![], &pos_map);
        println!("rebuilt:{rebuilt_ex}\n\npos:\n{pos_map:?}\n\n\n");

        assert_eq!(m, *expected);

        if *expected {
            assert_eq!(rebuilt_ex, ex.clone());
        }
    }
}
