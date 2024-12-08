%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FUN "fun"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token ADD "+"
%token AMP_AMP "&&"
%token ASTERISK "*"
%token BANG "!"
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token HYPHEN_GREATER "->"
%token PIPE_PIPE "||"
%token SEMICOLON ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.expr> main

%%

let main :=
| expr = located(expr); END;
    { expr }

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }

let binder :=
| located(NAME)

let param :=
| n = binder;
    { n, None }
| "("; n = binder; ":"; ty = located(ty); ")";
    { n, Some ty }

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.Fun_ty (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name n }

let expr :=
| "let"; n = binder; ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":=";
    expr0 = located(expr); ";"; expr1 = located(expr);
    { Surface.Let (n, ps, ty, expr0, expr1) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(expr);
    { Surface.Fun_lit (ps, t) }
| "if"; expr0 = located(or_expr); "then"; expr1 = located(expr); "else"; expr2 = located(expr);
    { Surface.If_then_else (expr0, expr1, expr2) }
| expr = located(or_expr); ":"; ty = located(ty);
    { Surface.Ann (expr, ty) }
| or_expr

let or_expr :=
| expr0 = located(add_expr); "||"; expr1 = located(or_expr);
    { Surface.Op2 (`Or, expr0, expr1) }
| and_expr

let and_expr :=
| expr0 = located(add_expr); "&&"; expr1 = located(and_expr);
    { Surface.Op2 (`And, expr0, expr1) }
| eq_expr

let eq_expr :=
| expr0 = located(add_expr); "="; expr1 = located(eq_expr);
    { Surface.Op2 (`Eq, expr0, expr1) }
| add_expr

let add_expr :=
| expr0 = located(mul_expr); "+"; expr1 = located(add_expr);
    { Surface.Op2 (`Add, expr0, expr1) }
| expr0 = located(mul_expr); "-"; expr1 = located(add_expr);
    { Surface.Op2 (`Sub, expr0, expr1) }
| mul_expr

let mul_expr :=
| expr0 = located(app_expr); "*"; expr1 = located(mul_expr);
    { Surface.Op2 (`Mul, expr0, expr1) }
| app_expr

let app_expr :=
| expr0 = located(app_expr); expr1 = located(atomic_expr);
    { Surface.App (expr0, expr1) }
| "-"; expr = located(atomic_expr);
    { Surface.Op1 (`Neg, expr) }
| "!"; expr = located(atomic_expr);
    { Surface.Op1 (`Not, expr) }
| atomic_expr

let atomic_expr :=
| "("; expr = expr; ")";
    { expr }
| n = NAME;
    { Surface.Name n }
| "true";
    { Surface.Bool_lit true }
| "false";
    { Surface.Bool_lit false }
| i = NUMBER;
    { Surface.Int_lit i }
