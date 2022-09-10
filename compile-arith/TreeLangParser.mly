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
| t = expr; END;
    { t }

let expr :=
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
| "-"; t = atomic_expr;
    { TreeLang.neg t }
