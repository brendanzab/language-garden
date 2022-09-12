%token <int> NUMBER
%token ADD "+"
%token ASTERISK "*"
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
| "-"; e = atomic_expr;
    { TreeLang.neg e }
