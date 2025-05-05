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
%token OPEN_BRACKET "["
%token CLOSE_BRACKET "]"
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
| n = NAME;
    { Some n }
| "_";
    { None }

let param :=
| "["; n = located(binder); "]";
    { Surface.Ty_param n }
| n = located(binder);
    { Surface.Param (n, None) }
| "("; n = located(binder); ":"; ty = located(ty); ")";
    { Surface.Param (n, Some ty) }

let ty :=
| ns = nonempty_list("["; ~ = located(binder); "]"; <>); "->"; ty = located(ty);
    { Surface.Forall_type (ns, ty) }
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.Fun_type (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name n }
| "_";
    { Surface.Placeholder }

let tm :=
| "let"; n = located(binder); ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":=";
    tm0 = located(tm); ";"; tm1 = located(tm);
    { Surface.Let (n, ps, ty, tm0, tm1) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.Fun_lit (ps, t) }
| "if"; tm0 = located(eq_tm); "then"; tm1 = located(tm); "else"; tm2 = located(tm);
    { Surface.If_then_else (tm0, tm1, tm2) }
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
| tm = located(app_tm); "["; ty = located(ty); "]";
    { Surface.App (tm, Ty_arg ty) }
| tm0 = located(app_tm); tm1 = located(atomic_tm);
    { Surface.App (tm0, Arg tm1) }
| "-"; tm = located(atomic_tm);
    { Surface.Op1 (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| "true";
    { Surface.Bool_lit true }
| "false";
    { Surface.Bool_lit false }
| i = NUMBER;
    { Surface.Int_lit i }
