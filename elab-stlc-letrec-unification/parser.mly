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
| tm = spanned(tm); END;
    { tm }


(* Types *)

let ty :=
| ty1 = spanned(atomic_ty); "->"; ty2 = spanned(ty);
    { Surface.Fun_type (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name n }
| UNDERSCORE;
    { Surface.Placeholder }


(* Terms *)

let tm :=
| "let"; d = defn; ";"; tm = spanned(tm);
    { Surface.Let (d, tm) }
| "let"; ds = nonempty_list("rec"; ~ = defn; ";"; <>); tm = spanned(tm);
    { Surface.Let_rec (ds, tm) }
| "fun"; ps = nonempty_list(param); "=>"; t = spanned(tm);
    { Surface.Fun_lit (ps, t) }
| "if"; tm1 = spanned(eq_tm); "then"; tm2 = spanned(tm); "else"; tm3 = spanned(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| tm = spanned(eq_tm); ":"; ty = spanned(ty);
    { Surface.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm1 = spanned(add_tm); "="; tm2 = spanned(eq_tm);
    { Surface.Infix (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = spanned(mul_tm); "+"; tm2 = spanned(add_tm);
    { Surface.Infix (`Add, tm1, tm2) }
| tm1 = spanned(mul_tm); "-"; tm2 = spanned(add_tm);
    { Surface.Infix (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = spanned(app_tm); "*"; tm2 = spanned(mul_tm);
    { Surface.Infix (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = spanned(app_tm); tm2 = spanned(atomic_tm);
    { Surface.App (tm1, tm2) }
| "-"; tm = spanned(atomic_tm);
    { Surface.Prefix (`Neg, tm) }
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

let defn :=
| n = spanned(binder); ps = list(param); ty = option(":"; ty = spanned(ty); { ty }); ":="; tm = spanned(tm);
    { n, ps, ty, tm }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.{ span = $loc; data } }
