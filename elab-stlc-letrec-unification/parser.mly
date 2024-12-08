%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FUN "fun"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token KEYWORD_REC "rec"
%token ADD "+"
%token ASTERISK "*"
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"
%token UNDERSCORE "_"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.tm> main

%%

let main :=
| tm = located(tm); END;
    { tm }

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

let defn :=
| n = binder; ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":="; tm = located(tm);
    { n, ps, ty, tm }

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.FunType (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name n }
| UNDERSCORE;
    { Surface.Placeholder }

let tm :=
| "let"; d = defn; ";"; tm = located(tm);
    { Surface.Let (d, tm) }
| "let"; ds = nonempty_list("rec"; ~ = defn; ";"; <>); tm = located(tm);
    { Surface.LetRec (ds, tm) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.FunLit (ps, t) }
| "if"; tm0 = located(eq_tm); "then"; tm1 = located(tm); "else"; tm2 = located(tm);
    { Surface.IfThenElse (tm0, tm1, tm2) }
| tm = located(eq_tm); ":"; ty = located(ty);
    { Surface.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm0 = located(add_tm); "="; tm1 = located(eq_tm);
    { Surface.Op2 (`Eq, tm0, tm1) }
| add_tm

let add_tm :=
| tm0 = located(mul_tm); "+"; tm1 = located(add_tm);
    { Surface.Op2 (`Add, tm0, tm1) }
| tm0 = located(mul_tm); "-"; tm1 = located(add_tm);
    { Surface.Op2 (`Sub, tm0, tm1) }
| mul_tm

let mul_tm :=
| tm0 = located(app_tm); "*"; tm1 = located(mul_tm);
    { Surface.Op2 (`Mul, tm0, tm1) }
| app_tm

let app_tm :=
| tm0 = located(app_tm); tm1 = located(atomic_tm);
    { Surface.App (tm0, tm1) }
| "-"; tm = located(atomic_tm);
    { Surface.Op1 (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| "true";
    { Surface.BoolLit true }
| "false";
    { Surface.BoolLit false }
| i = NUMBER;
    { Surface.IntLit i }
