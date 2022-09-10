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
| t = expr; END;
    { t }

let expr :=
| if_expr

let if_expr :=
| "if"; t1 = eq_expr; "then"; t2 = eq_expr; "else"; t3 = if_expr;
    { TreeLang.if_then_else t1 t2 t3 }
| eq_expr

let eq_expr :=
| t1 = add_expr; "="; t2 = eq_expr;
    { TreeLang.eq t1 t2 }
| add_expr

let add_expr :=
| t1 = mul_expr; "+"; t2 = add_expr;
    { TreeLang.add t1 t2 }
| t1 = mul_expr; "-"; t2 = add_expr;
    { TreeLang.sub t1 t2 }
| mul_expr

let mul_expr :=
| t1 = atomic_expr; "*"; t2 = mul_expr;
    { TreeLang.mul t1 t2 }
| t1 = atomic_expr; "/"; t2 = mul_expr;
    { TreeLang.div t1 t2 }
| atomic_expr

let atomic_expr :=
| "("; t = expr; ")";
    { t }
| n = NUMBER;
    { TreeLang.num n }
| "true";
    { TreeLang.bool true }
| "false";
    { TreeLang.bool false }
| "-"; t = atomic_expr;
    { TreeLang.neg t }
