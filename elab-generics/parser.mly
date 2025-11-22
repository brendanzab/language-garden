%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FUN "fun"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token ADD "+"
%token ASTERISK "*"
%token COLON ":"
%token COLON_EQUALS ":="
%token COMMA ","
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"
%token UNDERSCORE "_"
%token OPEN_BRACKET "["
%token CLOSE_BRACKET "]"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.Tm.t> main

%%

let main :=
| tm = spanned(tm); END;
    { tm }


(* Types *)

let ty :=
| ty1 = spanned(atomic_ty); "->"; ty2 = spanned(ty);
    { Surface.Ty.Fun (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Ty.Name n }
| UNDERSCORE;
    { Surface.Ty.Placeholder }

let tm :=
| "let"; d = def; ";"; tm = spanned(tm);
    { Surface.Tm.Let (d, tm) }
| "fun"; ps = nonempty_list(param); "=>"; tm = spanned(tm);
    { Surface.Tm.Fun (ps, tm) }
| "if"; tm1 = spanned(eq_tm); "then"; tm2 = spanned(tm); "else"; tm3 = spanned(tm);
    { Surface.Tm.If_then_else (tm1, tm2, tm3) }
| tm = spanned(eq_tm); ":"; ty = spanned(ty);
    { Surface.Tm.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm1 = spanned(add_tm); "="; tm2 = spanned(eq_tm);
    { Surface.Tm.Infix (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = spanned(mul_tm); "+"; tm2 = spanned(add_tm);
    { Surface.Tm.Infix (`Add, tm1, tm2) }
| tm1 = spanned(mul_tm); "-"; tm2 = spanned(add_tm);
    { Surface.Tm.Infix (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = spanned(app_tm); "*"; tm2 = spanned(mul_tm);
    { Surface.Tm.Infix (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| n = spanned(NAME); "["; tys = trailing_nonempty_list(",", spanned(ty)); "]";
    { Surface.Tm.Name (n, tys) }
| tm1 = spanned(app_tm); tm2 = spanned(atomic_tm);
    { Surface.Tm.App (tm1, tm2) }
| "-"; tm = spanned(atomic_tm);
    { Surface.Tm.Prefix (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = spanned(NAME);
    { Surface.Tm.Name (n, []) }
| i = NUMBER;
    { Surface.Tm.Int i }


(* Binders *)

let binder :=
| n = NAME;
    { Some n }
| "_";
    { None }

let param :=
| n = spanned(binder);
    { n, None }
| "("; n = spanned(binder); ":"; ty = spanned(ty); ")";
    { n, Some ty }


(* Definitions *)

let ty_params :=
| "["; tps = trailing_nonempty_list(",", spanned(NAME)); "]"; { tps }
| { [] }

let def :=
| n = spanned(binder); tps = ty_params;
    ps = list(param); ty = option(":"; ty = spanned(ty); { ty }); ":=";
    tm = spanned(tm);
    { n, tps, ps, ty, tm }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.Spanned.{ span = $loc; data } }

let trailing_list(Sep, T) :=
  | { [] }
  | trailing_nonempty_list(Sep, T)

let trailing_nonempty_list(Sep, T) :=
  | t = T; option(Sep);
      { [ t ] }
  | t = T; Sep; ts = trailing_nonempty_list(Sep, T);
      { t :: ts }
