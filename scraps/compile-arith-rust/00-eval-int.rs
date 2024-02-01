// Syntax

pub enum Expr {
  Int(i32),
  Neg(Box<Expr>),
  Add(Box<Expr>, Box<Expr>),
  Mul(Box<Expr>, Box<Expr>),
}

// Semantics

pub type Value = i32;

pub fn eval(expr: &Expr) -> Value {
    match expr {
        Expr::Int(i) => *i,
        Expr::Neg(e) => -eval(e),
        Expr::Add(e1, e2) => eval(e1) + eval(e2),
        Expr::Mul(e1, e2) => eval(e1) * eval(e2),
    }
}