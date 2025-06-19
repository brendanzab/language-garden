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


(* Types *)

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


(* Terms *)

let tm :=
| "let"; n = located(binder); ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":=";
    tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ps, ty, tm1, tm2) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.Fun_lit (ps, t) }
| "if"; tm1 = located(eq_tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| tm = located(eq_tm); ":"; ty = located(ty);
    { Surface.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm1 = located(add_tm); "="; tm2 = located(eq_tm);
    { Surface.Infix (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = located(mul_tm); "+"; tm2 = located(add_tm);
    { Surface.Infix (`Add, tm1, tm2) }
| tm1 = located(mul_tm); "-"; tm2 = located(add_tm);
    { Surface.Infix (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = located(app_tm); "*"; tm2 = located(mul_tm);
    { Surface.Infix (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| tm = located(app_tm); arg = located(arg);
    { Surface.App (tm, arg) }
| "-"; tm = located(atomic_tm);
    { Surface.Prefix (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| i = NUMBER;
    { Surface.Int_lit i }


(* Binders *)

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


(* Arguments *)

let arg :=
| "["; ty = located(ty); "]";
    { Surface.Ty_arg ty }
| tm = located(atomic_tm);
    { Surface.Arg tm }


(* Utilities *)

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
