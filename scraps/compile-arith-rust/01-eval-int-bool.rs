// Syntax

pub enum Expr {
    Int(i32),
    Bool(bool),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
}

// Semantics

#[derive(PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
}

pub fn eval(expr: &Expr) -> Value {
    match expr {
        Expr::Int(i) => Value::Int(*i),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Neg(e) => Value::Int(-eval_int(e)),
        Expr::Add(e1, e2) => Value::Int(eval_int(e1) + eval_int(e2)),
        Expr::Sub(e1, e2) => Value::Int(eval_int(e1) - eval_int(e2)),
        Expr::Mul(e1, e2) => Value::Int(eval_int(e1) * eval_int(e2)),
        Expr::Eq(e1, e2) => Value::Bool(eval(e1) == eval(e2)),
        Expr::And(e1, e2) => Value::Bool(eval_bool(e1) && eval_bool(e2)),
    }
}

fn eval_int(expr: &Expr) -> i32 {
    match eval(expr) {
        Value::Int(i) => i,
        _ => panic!("expected int"),
    }
}

fn eval_bool(expr: &Expr) -> bool {
    match eval(expr) {
        Value::Bool(b) => b,
        _ => panic!("expected bool"),
    }
}