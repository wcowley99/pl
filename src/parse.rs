use crate::{
    expr::{BinOp, Expr, ExprPool, ExprRef},
    lex::{Lex, Lexeme},
};

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
    ReduceInPlace,
    Error,
    Accept,
}

fn next_action(state: Option<&State>, token: Option<&Lexeme>) -> Action {
    use Lexeme::*;
    use State::*;

    match (state, token) {
        (None, Some(Num(_))) => Action::ShiftPush(MkNum),

        (Some(MkAdd) | Some(MkMul), Some(Num(_))) => Action::ShiftSame,

        (Some(MkNum), Some(Plus)) => Action::ShiftTo(MkAdd),
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
        (Some(MkNum), Some(In)) => Action::ReduceInPlace,

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
            Action::ReduceInPlace => {
                dbg!(&states, &terms, token);
                reduce(&mut pool, &mut terms, &mut states)?;
            }
            Action::Reduce => {
                dbg!(&states, &terms, token);
                reduce(&mut pool, &mut terms, &mut states)?;
                lex.consume();
            }
            Action::Accept => {
                let expr = pool.add(terms.pop().unwrap());
                if !terms.is_empty() {
                    return Err(format!("Dangling terms: {:?}", terms));
                } else {
                    return Ok(expr);
                }
            }
            Action::Error => return Err(format!("Unexpected token {:?}", token)),
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
    fn test_let_parsing() {
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
}
