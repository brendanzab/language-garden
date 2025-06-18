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
    expr1 = located(expr); ";"; expr2 = located(expr);
    { Surface.Let (n, ps, ty, expr1, expr2) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(expr);
    { Surface.Fun_lit (ps, t) }
| "if"; expr1 = located(or_expr); "then"; expr2 = located(expr); "else"; expr3 = located(expr);
    { Surface.If_then_else (expr1, expr2, expr3) }
| expr = located(or_expr); ":"; ty = located(ty);
    { Surface.Ann (expr, ty) }
| or_expr

let or_expr :=
| expr1 = located(add_expr); "||"; expr2 = located(or_expr);
    { Surface.Infix (`Or, expr1, expr2) }
| and_expr

let and_expr :=
| expr1 = located(add_expr); "&&"; expr2 = located(and_expr);
    { Surface.Infix (`And, expr1, expr2) }
| eq_expr

let eq_expr :=
| expr1 = located(add_expr); "="; expr2 = located(eq_expr);
    { Surface.Infix (`Eq, expr1, expr2) }
| add_expr

let add_expr :=
| expr1 = located(mul_expr); "+"; expr2 = located(add_expr);
    { Surface.Infix (`Add, expr1, expr2) }
| expr1 = located(mul_expr); "-"; expr2 = located(add_expr);
    { Surface.Infix (`Sub, expr1, expr2) }
| mul_expr

let mul_expr :=
| expr1 = located(app_expr); "*"; expr2 = located(mul_expr);
    { Surface.Infix (`Mul, expr1, expr2) }
| app_expr

let app_expr :=
| expr1 = located(app_expr); expr2 = located(atomic_expr);
    { Surface.App (expr1, expr2) }
| "-"; expr = located(atomic_expr);
    { Surface.Prefix (`Neg, expr) }
| "!"; expr = located(atomic_expr);
    { Surface.Prefix (`Not, expr) }
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
