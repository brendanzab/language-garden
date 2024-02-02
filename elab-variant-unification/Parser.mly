%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_END "end"
%token KEYWORD_FUN "fun"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_MATCH "match"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token KEYWORD_WITH "with"
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
%token VERTICAL_LINE "|"
%token OPEN_BRACE "["
%token CLOSE_BRACE "]"
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

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.FunType (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| "["; "|"; "]";
    { Surface.VariantType [] }
| "["; option("|"); cs = separated_nonempty_list("|", l = located(NAME); ":"; ty = located(ty); { l, ty }); "]";
    { Surface.VariantType cs }
| n = NAME;
    { Surface.Name n }
| UNDERSCORE;
    { Surface.Placeholder }

let pattern :=
| "["; l = located(NAME); ":="; n = located(NAME); "]";
    { Surface.VariantLit (l, n) : Surface.pattern_data }

let clause :=
| p = located(pattern); "=>"; tm = located(tm); { p, tm }

let tm :=
| "let"; n = binder; ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":=";
    tm0 = located(tm); ";"; tm1 = located(tm);
    { Surface.Let (n, ps, ty, tm0, tm1) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.FunLit (ps, t) }
| "if"; tm0 = located(eq_tm); "then"; tm1 = located(tm); "else"; tm2 = located(tm);
    { Surface.IfThenElse (tm0, tm1, tm2) }
| "match"; tm0 = located(eq_tm); "with"; option("|"); clauses = separated_list("|", clause); "end";
    { Surface.Match (tm0, clauses) }
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
    { Surface.FunApp (tm0, tm1) }
| "-"; tm = located(atomic_tm);
    { Surface.Op1 (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| "["; l = located(NAME); ":="; tm = located(tm); "]";
    { Surface.VariantLit (l, tm) }
| "true";
    { Surface.BoolLit true }
| "false";
    { Surface.BoolLit false }
| i = NUMBER;
    { Surface.IntLit i }
