%token <string> NAME
%token <string> PRIM
%token <int32> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FUN "fun"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token KEYWORD_VAL "val"
%token ADD "+"
%token ASTERISK "*"
%token COLON ":"
%token COLON_EQUALS ":="
%token COMMA ","
%token EQUALS "="
%token HYPHEN "-"
%token SEMICOLON ";"
%token UNDERSCORE "_"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.Program.t> main

%%

let main :=
| prog = program; END;
    { prog }


(* Types *)

let ty :=
| n = NAME;
    { Surface.Ty.Name n }


(* Terms *)

let tm :=
| "let"; n = spanned(pattern); ty = option(":"; ty = spanned(ty); { ty }); ":=";
    tm1 = spanned(tm); ";"; tm2 = spanned(tm);
    { Surface.Expr.Let (n, ty, tm1, tm2) }
| "if"; tm1 = spanned(eq_tm); "then"; tm2 = spanned(tm); "else"; tm3 = spanned(tm);
    { Surface.Expr.If_then_else (tm1, tm2, tm3) }
| tm = spanned(eq_tm); ":"; ty = spanned(ty);
    { Surface.Expr.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm1 = spanned(add_tm); "="; tm2 = spanned(eq_tm);
    { Surface.Expr.Infix (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = spanned(mul_tm); "+"; tm2 = spanned(add_tm);
    { Surface.Expr.Infix (`Add, tm1, tm2) }
| tm1 = spanned(mul_tm); "-"; tm2 = spanned(add_tm);
    { Surface.Expr.Infix (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = spanned(app_tm); "*"; tm2 = spanned(mul_tm);
    { Surface.Expr.Infix (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| "-"; tm = spanned(atomic_tm);
    { Surface.Expr.Prefix (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = spanned(NAME);
    { Surface.Expr.Name (n, None) }
| n = spanned(NAME); "("; args = separated_nonempty_sequence(",", spanned(tm)); ")";
    { Surface.Expr.Name (n, Some (Iarray.of_list args)) }
| n = spanned(PRIM); "("; args = separated_nonempty_sequence(",", spanned(tm)); ")";
    { Surface.Expr.Prim (n, Iarray.of_list args) }
| i = NUMBER;
    { Surface.Expr.I32 i }


(* Items *)

let item :=
| "val"; n = spanned(NAME); ":"; ty = spanned(ty); ":="; tm = spanned(tm);
    { Surface.Item.Val (n, ty, tm) }
| "fun"; n = spanned(NAME); "("; params = separated_nonempty_sequence(",", param); ")";
  ":"; ty = spanned(ty); ":="; tm = spanned(tm);
    { Surface.Item.Fun (n, Iarray.of_list params, ty, tm) }


(* Programs *)

let program :=
| items = list(i = item; ";"; { i });
    { items }


(* Binders *)

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| n = spanned(pattern); ":"; ty = spanned(ty);
    { n, ty }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.Spanned.{ span = $loc; data } }

let separated_nonempty_sequence(Sep, T) :=
| t = T; option(Sep);
    { [ t ] }
| t = T; Sep; ts = separated_nonempty_sequence(Sep, T);
    { t :: ts }
