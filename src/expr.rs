use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Add1(ExprRef),
    Binary(BinOp, ExprRef, ExprRef),
    Let(String, ExprRef, ExprRef),
    Id(String),
}

impl Expr {
    pub fn id<S: Into<String>>(name: S) -> Expr {
        Expr::Id(name.into())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprPool(pub Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn add(&mut self, expr: Expr) -> ExprRef {
        let index = self.0.len();
        self.0.push(expr);

        ExprRef(index)
    }

    pub fn get(&self, index: ExprRef) -> Result<&Expr, String> {
        self.0
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    pub fn subst(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> ExprRef {
        match self.get(expr).unwrap() {
            Expr::Id(id) => *env.get(id).unwrap(),
            _ => expr,
        }
    }

    pub fn eval(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> i64 {
        match self.get(expr).unwrap() {
            Expr::Num(val) => *val,
            Expr::Add1(expr) => 1 + self.eval(*expr, env),
            Expr::Let(id, expr, body) => {
                let mut env = env.clone();
                env.insert(id.clone(), self.subst(*expr, &env));

                self.eval(*body, &env)
            }
            Expr::Id(id) => self.eval(env.get(id).unwrap().clone(), env),
            Expr::Binary(op, lhs, rhs) => todo!(),
            // Expr::If(cond, body, branch) => {
            //     if cond.eval(env) != 0 {
            //         body.eval(env)
            //     } else {
            //         branch.eval(env)
            //     }
            // }
        }
    }

    pub fn interp(&self, start: ExprRef) -> i64 {
        let env = HashMap::new();
        self.eval(start, &env)
    }
}

#[cfg(test)]
mod test {
    use crate::{expr::ExprPool, lex::Lex, parse};

    #[test]
    fn source_to_interp2() {
        let lex = Lex::new("let x = 5 in x");
        let mut pool = ExprPool::new();
        let start = parse(&mut pool, lex).unwrap();

        assert_eq!(pool.interp(start), 5);
    }

    #[test]
    fn source_to_interp3() {
        let lex = Lex::new("if 5 then 1 else 2");
        let mut pool = ExprPool::new();
        let start = parse(&mut pool, lex).unwrap();

        assert_eq!(pool.interp(start), 1);
    }
}
