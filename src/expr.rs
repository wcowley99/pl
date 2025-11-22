use std::{iter::Peekable, os::linux::raw::stat};

use crate::parser::{Lex, Lexeme};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Binary(BinOp, ExprRef, ExprRef),
    Let(String, ExprRef, ExprRef),
    Id(String),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    MkNum,
    MkAdd,
    MkMul,
}

enum Action {
    ShiftPush(State),
    ShiftTo(State),
    ShiftSame,
    ReduceTo(State),
    Error,
}

fn next_action(state: Option<State>, token: &Lexeme) -> Action {
    match (state, token) {
        (None, Lexeme::Num(_)) => Action::ShiftPush(State::MkNum),

        (Some(State::MkAdd) | Some(State::MkMul), Lexeme::Num(_)) => Action::ShiftSame,

        (Some(State::MkNum), Lexeme::Plus) => Action::ShiftTo(State::MkAdd),
        (Some(State::MkAdd) | Some(State::MkMul), Lexeme::Plus) => Action::ReduceTo(State::MkAdd),

        (Some(State::MkNum), Lexeme::Star) => Action::ShiftTo(State::MkMul),
        (Some(State::MkAdd), Lexeme::Star) => Action::ShiftPush(State::MkMul),
        (Some(State::MkMul), Lexeme::Star) => Action::ReduceTo(State::MkMul),

        (None, Lexeme::Plus | Lexeme::Star) => Action::Error,
        _ => Action::Error,
    }
}

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, expr: Expr) -> ExprRef {
        let index = self.0.len();
        self.0.push(expr);

        ExprRef(index)
    }

    fn get(&mut self, index: ExprRef) -> Result<&Expr, String> {
        self.0
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    pub fn parse(&mut self, mut lex: Lex) -> Result<ExprRef, String> {
        self.expr_parse(&mut lex)
    }

    fn reduce(&mut self, terms: &mut Vec<Expr>, states: &mut Vec<State>) -> Result<(), String> {
        match states.pop() {
            Some(State::MkAdd) => {
                let e2 = terms.pop().unwrap();
                let e1 = terms.pop().unwrap();

                terms.push(Expr::Binary(BinOp::Add, self.add(e1), self.add(e2)));
                Ok(())
            }
            Some(State::MkMul) => {
                let e2 = terms.pop().unwrap();
                let e1 = terms.pop().unwrap();

                terms.push(Expr::Binary(BinOp::Mul, self.add(e1), self.add(e2)));
                Ok(())
            }
            Some(State::MkNum) => Ok(()),
            s => Err(format!("Attempted to reduce irreducible state {:?}", s)),
        }
    }

    fn expr_parse(&mut self, lex: &mut Lex) -> Result<ExprRef, String> {
        if let None = lex.peek() {
            return Err("No tokens".into());
        }

        let mut terms = Vec::new();
        let mut states = Vec::new();

        while let Some(token) = lex.next() {
            match (states.last(), &token) {
                (None, Lexeme::Num(n)) => {
                    terms.push(Expr::Num(*n));
                    states.push(State::MkNum);
                }
                (Some(State::MkAdd), Lexeme::Num(n)) => terms.push(Expr::Num(*n)),
                (Some(State::MkMul), Lexeme::Num(n)) => terms.push(Expr::Num(*n)),
                (Some(State::MkNum), Lexeme::Plus) => {
                    states.pop();
                    states.push(State::MkAdd);
                }
                (Some(State::MkAdd), Lexeme::Plus) | (Some(State::MkMul), Lexeme::Plus) => {
                    self.reduce(&mut terms, &mut states)?;
                    states.push(State::MkAdd);
                }
                (Some(State::MkNum), Lexeme::Star) => {
                    states.pop();
                    states.push(State::MkMul);
                }
                (Some(State::MkAdd), Lexeme::Star) => states.push(State::MkMul),
                (Some(State::MkMul), Lexeme::Star) => {
                    self.reduce(&mut terms, &mut states)?;
                    states.push(State::MkMul);
                }
                (None, Lexeme::Plus) | (None, Lexeme::Star) => {
                    return Err(format!("Unexpected token {:?}", token));
                }
                s => return Err(format!("Unexpected {:?}", s)),
            }
        }

        while !states.is_empty() {
            self.reduce(&mut terms, &mut states)?;
        }

        let expr = self.add(terms.pop().unwrap());
        if !terms.is_empty() {
            Err(format!("Dangling terms: {:?}", terms))
        } else {
            Ok(expr)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expr::{BinOp, Expr, ExprPool, ExprRef},
        parser::{Lex, Lexeme},
    };

    #[test]
    fn test_parse_int_literal() {
        let mut pool = ExprPool::new();
        let lex = Lex::new(vec![Lexeme::Num(13)]);
        let result = pool.parse(lex);

        assert_eq!(result, Ok(ExprRef(0)));
        assert_eq!(pool.0, vec![Expr::Num(13)]);
    }

    #[test]
    fn test_parse_binary_operation() {
        let mut pool = ExprPool::new();
        let lex = Lex::new(vec![Lexeme::Num(12), Lexeme::Plus, Lexeme::Num(25)]);
        let result = pool.parse(lex);

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
        let lex = Lex::new(vec![
            Lexeme::Num(12),
            Lexeme::Plus,
            Lexeme::Num(25),
            Lexeme::Plus,
            Lexeme::Num(9),
        ]);
        let result = pool.parse(lex);

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
        let lex = Lex::new(vec![
            Lexeme::Num(12),
            Lexeme::Plus,
            Lexeme::Num(25),
            Lexeme::Star,
            Lexeme::Num(9),
        ]);
        let result = pool.parse(lex);

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
}
