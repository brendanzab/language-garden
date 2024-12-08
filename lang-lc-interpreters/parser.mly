%token <string> NAME
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
%token COLON_EQUALS ":="
%token EQUALS_GREATER "=>"
%token SEMICOLON ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Named.expr> main

%%

let main :=
| e = expr; END;
    { e }

let expr :=
| "let"; n = NAME; ":="; e1 = expr; ";"; e2 = expr;
    { Named.Let (n, e1, e2) }
| "fun"; n = NAME; "=>"; e = expr;
    { Named.Fun_lit (n, e) }
| app_expr

let app_expr :=
| e1 = app_expr; e2 = atomic_expr;
    { Named.Fun_app (e1, e2) }
| atomic_expr

let atomic_expr :=
| "("; e = expr; ")";
    { e }
| n = NAME;
    { Named.Var n }
