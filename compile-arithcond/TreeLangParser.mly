%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token ADD "+"
%token ASTERISK "*"
%token EQUALS "="
%token FORWARD_SLASH "/"
%token HYPHEN "-"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <TreeLang.expr> main

%%

let main :=
| e = expr; END;
    { e }

let expr :=
| if_expr

let if_expr :=
| "if"; e1 = eq_expr; "then"; e2 = eq_expr; "else"; e3 = if_expr;
    { TreeLang.if_then_else e1 e2 e3 }
| eq_expr

let eq_expr :=
| e1 = add_expr; "="; e2 = eq_expr;
    { TreeLang.eq e1 e2 }
| add_expr

let add_expr :=
| e1 = mul_expr; "+"; e2 = add_expr;
    { TreeLang.add e1 e2 }
| e1 = mul_expr; "-"; e2 = add_expr;
    { TreeLang.sub e1 e2 }
| mul_expr

let mul_expr :=
| e1 = atomic_expr; "*"; e2 = mul_expr;
    { TreeLang.mul e1 e2 }
| e1 = atomic_expr; "/"; e2 = mul_expr;
    { TreeLang.div e1 e2 }
| atomic_expr

let atomic_expr :=
| "("; e = expr; ")";
    { e }
| n = NUMBER;
    { TreeLang.num n }
| "true";
    { TreeLang.bool true }
| "false";
    { TreeLang.bool false }
| "-"; e = atomic_expr;
    { TreeLang.neg e }
