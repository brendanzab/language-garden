pub mod tree {
    // Syntax

    pub enum Expr {
        Int(i32),
        Neg(Box<Expr>),
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
    }

    // Semantics

    pub type Value = i32;

    pub fn eval(expr: &Expr) -> Value {
        match expr {
            Expr::Int(i) => *i,
            Expr::Neg(e) => -eval(e),
            Expr::Add(e1, e2) => eval(e1) + eval(e2),
            Expr::Sub(e1, e2) => eval(e1) - eval(e2),
            Expr::Mul(e1, e2) => eval(e1) * eval(e2),
        }
    }
}

pub mod stack {
    // Syntax

    pub enum Inst {
        Int(i32),   // (      -- i    )
        Neg,        // (i     -- -n   )
        Add,        // (i1 i2 -- i1+i2)
        Sub,        // (i1 i2 -- i1-i2)
        Mul,        // (i1 i2 -- i1*i2)
    }

    pub type Code = Vec<Inst>;

    // Semantics

    pub type Value = i32;
    pub type Stack = Vec<Value>;

    pub fn eval(code: &Code) -> Stack {
        let mut stack = Vec::new();
        for inst in code {
            match inst {
                Inst::Int(i) => { 
                    stack.push(*i);
                }
                Inst::Neg => {
                    let i = stack.pop().unwrap();
                    stack.push(-i);
                }
                Inst::Add => {
                    let i2 = stack.pop().unwrap();
                    let i1 = stack.pop().unwrap();
                    stack.push(i1 + i2);
                }
                Inst::Sub => {
                    let i2 = stack.pop().unwrap();
                    let i1 = stack.pop().unwrap();
                    stack.push(i1 - i2);
                }
                Inst::Mul => {
                    let i2 = stack.pop().unwrap();
                    let i1 = stack.pop().unwrap();
                    stack.push(i1 * i2);
                }
            }
        }
        stack
    }
}

fn compile_acc(code: &mut stack::Code, expr: &tree::Expr) {
    match expr {
        tree::Expr::Int(i) => {
            code.push(stack::Inst::Int(*i));
        },
        tree::Expr::Neg(e) => {
            compile_acc(code, e);
            code.push(stack::Inst::Neg);
        },
        tree::Expr::Add(e1, e2) => {
            compile_acc(code, e1);
            compile_acc(code, e2);
            code.push(stack::Inst::Add);
        },
        tree::Expr::Sub(e1, e2) => {
            compile_acc(code, e1);
            compile_acc(code, e2);
            code.push(stack::Inst::Sub);
        },
        tree::Expr::Mul(e1, e2) => {
            compile_acc(code, e1);
            compile_acc(code, e2);
            code.push(stack::Inst::Mul);
        },
    }
}

pub fn compile(expr: &tree::Expr) -> stack::Code {
    let mut code = Vec::new();
    compile_acc(&mut code, expr);
    code
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: property based tests

    #[test]
    fn test_compile() {
        use tree::Expr::*;

        let tree_expr = Add(
            Box::new(Sub(
                Box::new(Int(42)),
                Box::new(Int(3)),
            )),
            Box::new(Mul(
                Box::new(Int(7)),
                Box::new(Int(3)),
            )),
        );

        let stack_code = compile(&tree_expr);

        assert_eq!(
            vec![tree::eval(&tree_expr)],
            stack::eval(&stack_code),
        );
    }
}