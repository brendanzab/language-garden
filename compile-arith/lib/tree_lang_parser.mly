%token <int> NUMBER
%token ADD "+"
%token ASTERISK "*"
%token FORWARD_SLASH "/"
%token HYPHEN "-"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Tree_lang.expr> main

%%

let main :=
| e = expr; END;
    { e }

let expr :=
| add_expr

let add_expr :=
| e1 = mul_expr; "+"; e2 = add_expr;
    { Tree_lang.add e1 e2 }
| e1 = mul_expr; "-"; e2 = add_expr;
    { Tree_lang.sub e1 e2 }
| mul_expr

let mul_expr :=
| e1 = atomic_expr; "*"; e2 = mul_expr;
    { Tree_lang.mul e1 e2 }
| e1 = atomic_expr; "/"; e2 = mul_expr;
    { Tree_lang.div e1 e2 }
| atomic_expr

let atomic_expr :=
| "("; e = expr; ")";
    { e }
| i = NUMBER;
    { Tree_lang.int i }
| "-"; e = atomic_expr;
    { Tree_lang.neg e }
