//! Bidirectional elaborator for a simple functional language

use std::rc::Rc;

/// Surface types
enum Type {
    Name(String),
    Fun(Box<Type>, Box<Type>),
}

/// Surface terms
enum Term {
    Name(String),
    Ann(Box<Term>, Box<Type>),
    Let(String, Box<Term>, Option<Type>, Box<Term>),
    BoolLit(bool),
    IntLit(i32),
    FunLit(String, Option<Type>, Box<Term>),
    FunApp(Box<Term>, Box<Term>),
}

/// Desugared, explicitly typed core language
pub mod core {
    use std::rc::Rc;

    #[derive(Clone, PartialEq, Eq)]
    pub enum Type {
        Bool,
        Int,
        Fun(Rc<Type>, Rc<Type>),
    }

    pub enum Term {
        Var(usize), // Variable occurrences, represented as De Bruijn indices
        Let(String, Box<Term>, Rc<Type>, Box<Term>),
        BoolLit(bool),
        IntLit(i32),
        FunLit(String, Rc<Type>, Box<Term>),
        FunApp(Box<Term>, Box<Term>),
    }
}

/// A stack of bindings currently in scope
type Context = Vec<(String, Rc<core::Type>)>;

/// Elaborate a type into the core language
fn elab_type(r#type: &Type) -> Result<Rc<core::Type>, &'static str> {
    match r#type {
        Type::Name(name) => match name.as_str() {
            "Bool" => Ok(Rc::new(core::Type::Bool)),
            "Int" => Ok(Rc::new(core::Type::Int)),
            _ => Err("unbound type"),
        },
        Type::Fun(param_type, body_type) => Ok(Rc::new(core::Type::Fun(
            elab_type(param_type)?,
            elab_type(body_type)?,
        ))),
    }
}

/// Check a term against a type annotation, elaborating it into the core language
fn elab_check(
    context: &mut Context,
    term: &Term,
    expected_type: &Rc<core::Type>,
) -> Result<core::Term, &'static str> {
    match (term, expected_type.as_ref()) {
        (Term::Let(name, def, def_type, body), _) => {
            let (def, def_type) = elab_def(context, def, def_type)?;

            context.push((name.clone(), def_type.clone()));
            let body_result = elab_check(context, body, expected_type);
            context.pop();

            Ok(core::Term::Let(
                name.clone(),
                Box::new(def),
                def_type,
                Box::new(body_result?),
            ))
        }
        (Term::FunLit(name, param_type, body), core::Type::Fun(expected_param_type, body_type)) => {
            let param_type = match param_type {
                Some(param_type) => {
                    let param_type = elab_type(param_type)?;
                    match param_type == *expected_param_type {
                        true => param_type,
                        false => return Err("mismatched parameter type"),
                    }
                }
                None => expected_param_type.clone(),
            };

            context.push((name.clone(), param_type.clone()));
            let body_result = elab_check(context, body, body_type);
            context.pop();

            Ok(core::Term::FunLit(
                name.clone(),
                param_type,
                Box::new(body_result?),
            ))
        }
        // Switch to synthesis mode
        (term, _) => match elab_synth(context, term)? {
            (term, r#type) if r#type == *expected_type => Ok(term),
            _ => Err("mismatched types"),
        },
    }
}

/// Synthesise the type of a term, elaborating it into the core language
fn elab_synth(
    context: &mut Context,
    term: &Term,
) -> Result<(core::Term, Rc<core::Type>), &'static str> {
    match term {
        Term::Name(name) => {
            match (context.iter().rev().enumerate()).find(|(_, (n, _))| n == name) {
                Some((i, (_, r#type))) => Ok((core::Term::Var(i), r#type.clone())),
                None => Err("unbound variable"),
            }
        }
        Term::Ann(term, r#type) => {
            let r#type = elab_type(r#type)?;
            let term = elab_check(context, term, &r#type)?;
            Ok((term, r#type.clone()))
        }
        Term::Let(name, def, def_type, body) => {
            let (def, def_type) = elab_def(context, def, def_type)?;

            context.push((name.clone(), def_type.clone()));
            let body_result = elab_synth(context, body);
            context.pop();

            let (body, body_type) = body_result?;

            Ok((
                core::Term::Let(name.clone(), Box::new(def), def_type, Box::new(body)),
                body_type,
            ))
        }
        Term::BoolLit(b) => Ok((core::Term::BoolLit(*b), (Rc::new(core::Type::Bool)))),
        Term::IntLit(i) => Ok((core::Term::IntLit(*i), (Rc::new(core::Type::Int)))),
        Term::FunLit(name, Some(param_type), body) => {
            let param_type = elab_type(param_type)?;

            context.push((name.clone(), param_type.clone()));
            let body_result = elab_synth(context, body);
            context.pop();

            let (body, body_type) = body_result?;

            Ok((
                core::Term::FunLit(name.clone(), param_type.clone(), Box::new(body)),
                Rc::new(core::Type::Fun(param_type, body_type)),
            ))
        }
        Term::FunLit(_, None, _) => Err("ambiguous function literal"),
        Term::FunApp(head, arg) => {
            let (head, head_type) = elab_synth(context, head)?;
            match head_type.as_ref() {
                core::Type::Fun(param_type, body_type) => {
                    let arg = elab_check(context, arg, &param_type)?;
                    Ok((
                        core::Term::FunApp(Box::new(head), Box::new(arg)),
                        body_type.clone(),
                    ))
                }
                _ => Err("not a function"),
            }
        }
    }
}

/// Elaborate a definition with an optional type annotation.
fn elab_def(
    context: &mut Context,
    def: &Term,
    def_type: &Option<Type>,
) -> Result<(core::Term, Rc<core::Type>), &'static str> {
    match def_type {
        Some(def_type) => {
            let def_type = elab_type(def_type)?;
            Ok((elab_check(context, def, &def_type)?, def_type))
        }
        None => elab_synth(context, def),
    }
}
