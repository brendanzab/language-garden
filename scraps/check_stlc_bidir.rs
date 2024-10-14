//! Bidirectional type checker for a simple functional language
//!
//! Originally posted at <https://gist.github.com/brendanzab/36ed5eaf5eef58db577624ac83d72006>.

use std::rc::Rc;

#[derive(Clone, PartialEq, Eq)]
enum Type {
    Bool,
    Int,
    Fun(Rc<Type>, Rc<Type>),
}

enum Term {
    Var(String),
    Ann(Box<Term>, Rc<Type>),
    Let(String, Box<Term>, Box<Term>),
    BoolLit(bool),
    IntLit(i32),
    FunLit(String, Box<Term>),
    FunApp(Box<Term>, Box<Term>),
}

/// A stack of bindings currently in scope
type Context = Vec<(String, Rc<Type>)>;

/// Check a term against a type annotation
fn check(context: &mut Context, term: &Term, expected_type: &Rc<Type>) -> Result<(), &'static str> {
    match (term, expected_type.as_ref()) {
        (Term::Let(name, def, body), _) => {
            let def_type = synth(context, def)?;

            context.push((name.clone(), def_type));
            let body_result = check(context, body, expected_type);
            context.pop();

            body_result
        }
        (Term::FunLit(name, body), Type::Fun(param_type, body_type)) => {
            context.push((name.clone(), param_type.clone()));
            let body_result = check(context, body, body_type);
            context.pop();

            body_result
        }
        // Switch to synthesis mode
        (term, _) => match synth(context, term)? == *expected_type {
            true => Ok(()),
            false => Err("mismatched types"),
        },
    }
}

/// Synthesise the type of a term
fn synth(context: &mut Context, term: &Term) -> Result<Rc<Type>, &'static str> {
    match term {
        Term::Var(name) => match context.iter().rev().find(|(n, _)| n == name) {
            Some((_, r#type)) => Ok(r#type.clone()),
            None => Err("unbound variable"),
        },
        Term::Ann(term, r#type) => {
            check(context, term, r#type)?;
            Ok(r#type.clone())
        }
        Term::Let(name, def, body) => {
            let def_type = synth(context, def)?;

            context.push((name.clone(), def_type));
            let body_type = synth(context, body);
            context.pop();

            body_type
        }
        Term::BoolLit(_) => Ok(Rc::new(Type::Bool)),
        Term::IntLit(_) => Ok(Rc::new(Type::Int)),
        Term::FunLit(_, _) => Err("ambiguous function literal"),
        Term::FunApp(head, arg) => match synth(context, head)?.as_ref() {
            Type::Fun(param_type, body_type) => {
                check(context, arg, param_type)?;
                Ok(body_type.clone())
            }
            _ => Err("not a function"),
        },
    }
}
