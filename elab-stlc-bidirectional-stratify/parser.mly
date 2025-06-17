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
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.tm> main

%%

let main :=
| tm = located(tm); END;
    { tm }


(* Terms *)

let tm :=
| "let"; n = binder; ps = list(param); ty = option(":"; ty = located(tm); { ty }); ":=";
    tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ps, ty, tm1, tm2) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.Fun_lit (ps, t) }
| "if"; tm1 = located(eq_tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| tm = located(eq_tm); ":"; ty = located(tm);
    { Surface.Ann (tm, ty) }
| tm1 =  located(eq_tm); "->"; tm2 = located(tm);
    { Surface.Op2 (`Arrow, tm1, tm2) }
| eq_tm

let eq_tm :=
| tm1 = located(add_tm); "="; tm2 = located(eq_tm);
    { Surface.Op2 (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = located(mul_tm); "+"; tm2 = located(add_tm);
    { Surface.Op2 (`Add, tm1, tm2) }
| tm1 = located(mul_tm); "-"; tm2 = located(add_tm);
    { Surface.Op2 (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = located(app_tm); "*"; tm2 = located(mul_tm);
    { Surface.Op2 (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = located(app_tm); tm2 = located(atomic_tm);
    { Surface.App (tm1, tm2) }
| "-"; tm = located(atomic_tm);
    { Surface.Op1 (`Neg, tm) }
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
| located(NAME)

let param :=
| n = binder;
    { n, None }
| "("; n = binder; ":"; ty = located(tm); ")";
    { n, Some ty }


(* Utilities *)

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
