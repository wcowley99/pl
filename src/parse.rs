use std::collections::{HashMap, HashSet};

use crate::{
    expr::{BinOp, Expr, ExprPool, ExprRef},
    lex::{Lex, Lexeme},
};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Terminal {
    Let,
    Id,
    Eq,
    In,
    Plus,
    Star,
    Num,
    LParen,
    RParen,
    Eof,
}

impl Into<Token> for Terminal {
    fn into(self) -> Token {
        Token::Terminal(self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum NonTerminal {
    Start,
    Expr,
    Factor,
    Term,
}

impl Into<Token> for NonTerminal {
    fn into(self) -> Token {
        Token::NonTerminal(self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Token {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

struct Rule {
    pub symbol: NonTerminal,
    pub expr: Vec<Token>,
}

impl Rule {
    pub fn new(symbol: NonTerminal, expr: Vec<Token>) -> Self {
        Self { symbol, expr }
    }
}

// fn gen_first_table(rules: &Vec<Rule>) -> HashMap<NonTerminal, HashSet<Terminal>> {
//     let mut table: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
//     let mut changed = true;
//     while changed {
//         changed = false;
//         for rule in rules {
//             let lhs = rule.symbol;
//             let first = rule.expr[0];
//             match first {
//                 Token::Terminal(t) => match table.get(&lhs) {
//                     Some(terms) => changed |= terms.insert(t),
//                     None => changed |= table.insert(lhs, HashSet::from([t])).is_none(),
//                 },
//                 Token::NonTerminal(t) => {
//                     let terms = first
//                         .iter()
//                         .filter(|(nt, _)| *nt == t)
//                         .flat_map(|(nt, term)| {
//                             if *nt == t {
//                                 (nt, term)
//                             } else {
//                             }
//                         })
//                         .collect::<Vec<_>>();
//
//                     for term in terms {
//                         if first.contains(&(rule.symbol, term)) {
//                             first.push((rule.symbol, term));
//                         } else {
//                             changed = true;
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     return table;
// }
//
// fn temp() {
//     use NonTerminal::*;
//     use Terminal::*;
//     let rules = vec![
//         Rule::new(Start, vec![Expr.into()]),
//         Rule::new(Expr, vec![Expr.into(), Plus.into(), Factor.into()]),
//         Rule::new(Expr, vec![Factor.into()]),
//         Rule::new(Factor, vec![Factor.into(), Star.into(), Term.into()]),
//         Rule::new(Factor, vec![Term.into()]),
//         Rule::new(Term, vec![LParen.into(), Expr.into(), RParen.into()]),
//         Rule::new(Term, vec![Num.into()]),
//         Rule::new(Term, vec![Id.into()]),
//     ];
//
//     let first = gen_first_table(&rules);
// }

#[derive(Debug, Clone, PartialEq, Eq)]
enum State {
    MkNum,
    MkAdd,
    MkMul,
    MkLet(String),
    MkVar,
    MkLetId,
    MkLetEq,
    MkLetBind(String),
    MkLetIn,
    MkLetBody(String, Expr),
}

enum Action {
    ShiftPush(State),
    ShiftTo(State),
    ShiftSame,
    MoveTo(State),
    ReduceTo(State),
    Reduce,
    Error,
    Accept,
}

fn next_action(state: Option<&State>, token: Option<&Lexeme>) -> Action {
    use Lexeme::*;
    use State::*;

    match (state, token) {
        (None, Some(Num(_))) => Action::ShiftPush(MkNum),

        (Some(MkAdd) | Some(MkMul), Some(Num(_))) => Action::ShiftSame,

        (Some(MkNum) | Some(MkVar), Some(Plus)) => Action::ShiftTo(MkAdd),
        (Some(MkAdd) | Some(MkMul), Some(Plus)) => Action::ReduceTo(MkAdd),

        (Some(MkNum), Some(Star)) => Action::ShiftTo(MkMul),
        (Some(MkAdd), Some(Star)) => Action::ShiftPush(MkMul),
        (Some(MkMul), Some(Star)) => Action::ReduceTo(MkMul),

        (
            None
            | Some(MkNum)
            | Some(MkAdd)
            | Some(MkMul)
            | Some(MkLetBind(_))
            | Some(MkLetBody(_, _)),
            Some(Let),
        ) => Action::ShiftPush(MkLetId),
        (Some(MkLetId), Some(Id(id))) => Action::MoveTo(MkLet(id.clone())),
        (Some(MkLet(id)), Some(Eq)) => Action::ShiftTo(MkLetBind(id.clone())),
        (Some(MkLetBind(_)), Some(In)) => Action::Reduce,
        (Some(MkNum) | Some(MkVar) | Some(MkAdd) | Some(MkMul), Some(In)) => Action::Reduce,
        (Some(MkLetBody(_, _)), Some(In)) => Action::ShiftSame,

        (Some(MkLetBind(_)), Some(Num(_))) => Action::ShiftPush(MkNum),
        (Some(MkLetBody(_, _)), Some(Num(_))) => Action::ShiftPush(MkNum),

        (_, Some(Id(_))) => Action::ShiftPush(MkVar),

        (Some(_), None) => Action::Reduce,
        (None, None) => Action::Accept,

        (None, Some(Plus) | Some(Star)) => Action::Error,
        _ => Action::Error,
    }
}

pub fn parse(mut pool: &mut ExprPool, mut lex: Lex) -> Result<ExprRef, String> {
    expr_parse(&mut pool, &mut lex)
}

fn reduce(
    pool: &mut ExprPool,
    terms: &mut Vec<Expr>,
    states: &mut Vec<State>,
) -> Result<(), String> {
    match states.pop() {
        Some(State::MkAdd) => {
            let e2 = terms.pop().unwrap();
            let e1 = terms.pop().unwrap();

            terms.push(Expr::Binary(BinOp::Add, pool.add(e1), pool.add(e2)));
            Ok(())
        }
        Some(State::MkMul) => {
            let e2 = terms.pop().unwrap();
            let e1 = terms.pop().unwrap();

            terms.push(Expr::Binary(BinOp::Mul, pool.add(e1), pool.add(e2)));
            Ok(())
        }
        Some(State::MkLetBody(id, assn)) => {
            let body = terms.pop().unwrap();

            terms.push(Expr::Let(id, pool.add(assn), pool.add(body)));
            Ok(())
        }
        Some(State::MkLetBind(id)) => {
            let assn = terms.pop().unwrap();

            states.push(State::MkLetBody(id, assn));
            Ok(())
        }
        Some(State::MkVar) => Ok(()),
        Some(State::MkNum) => Ok(()),
        s => Err(format!("Attempted to reduce irreducible state {:?}", s)),
    }
}

fn lexeme_push(terms: &mut Vec<Expr>, token: Option<&Lexeme>) {
    match token {
        Some(Lexeme::Num(n)) => terms.push(Expr::Num(*n)),
        Some(Lexeme::Id(id)) => terms.push(Expr::Id(id.clone())),
        _ => (),
    }
}

fn expr_parse(mut pool: &mut ExprPool, lex: &mut Lex) -> Result<ExprRef, String> {
    if let None = lex.peek() {
        return Err("No tokens".into());
    }

    let mut terms = Vec::new();
    let mut states = Vec::new();

    loop {
        let token = lex.peek();
        match next_action(states.last(), token) {
            Action::ShiftPush(s) => {
                lexeme_push(&mut terms, token);

                states.push(s);
                lex.consume();
            }
            Action::ShiftTo(s) => {
                lexeme_push(&mut terms, token);

                if let Some(last) = states.last_mut() {
                    *last = s;
                }
                lex.consume();
            }
            Action::ShiftSame => {
                dbg!("ShiftSame----------------", &states, &terms, token);
                lexeme_push(&mut terms, token);
                lex.consume();
            }
            Action::MoveTo(s) => {
                if let Some(last) = states.last_mut() {
                    *last = s;
                }
                lex.consume();
            }
            Action::ReduceTo(s) => {
                reduce(&mut pool, &mut terms, &mut states)?;

                states.push(s);
                lex.consume();
            }
            Action::Reduce => {
                dbg!("Reduce----------------", &states, &terms, token);
                reduce(&mut pool, &mut terms, &mut states)?;
            }
            Action::Accept => {
                let expr = pool.add(terms.pop().unwrap());
                if !terms.is_empty() {
                    return Err(format!("Dangling terms: {:?}", terms));
                } else {
                    return Ok(expr);
                }
            }
            Action::Error => {
                dbg!("Error----------------", &states, &terms, token);
                return Err(format!("Unexpected token {:?}", token));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expr::{BinOp, Expr, ExprPool, ExprRef},
        lex::Lex,
        parse::parse,
    };

    #[test]
    fn test_parse_int_literal() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("13");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(0)));
        assert_eq!(pool, ExprPool(vec![Expr::Num(13)]));
    }

    #[test]
    fn test_parse_binary_operation() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("12 + 25");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(2)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(12),
                Expr::Num(25),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1))
            ]
        )
    }

    #[test]
    fn test_add_ops_left_associative() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("12 + 25 + 9");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(4)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(12),
                Expr::Num(25),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1)),
                Expr::Num(9),
                Expr::Binary(BinOp::Add, ExprRef(2), ExprRef(3)),
            ]
        )
    }

    #[test]
    fn test_bin_ops_observe_precedence() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("12 + 25 * 9");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(4)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(25),
                Expr::Num(9),
                Expr::Num(12),
                Expr::Binary(BinOp::Mul, ExprRef(0), ExprRef(1)),
                Expr::Binary(BinOp::Add, ExprRef(2), ExprRef(3)),
            ]
        )
    }

    #[test]
    fn test_let_basic() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("let x = 5 in x");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(2)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(5),
                Expr::id("x"),
                Expr::Let("x".into(), ExprRef(0), ExprRef(1)),
            ]
        );
    }

    #[test]
    fn test_let_as_term() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("5 + let x = 5 in x + 1");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(6)));
        assert_eq!(
            pool.0,
            vec![
                Expr::id("x"),
                Expr::Num(1),
                Expr::Num(5),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1)),
                Expr::Num(5),
                Expr::Let("x".into(), ExprRef(2), ExprRef(3)),
                Expr::Binary(BinOp::Add, ExprRef(4), ExprRef(5))
            ]
        );
    }
}
