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
%token COLON_EQUALS ":="
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token SEMICOLON ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.tm> main

%%

let main :=
| tm = located(tm); END;
    { tm }


let tm :=
| "let"; n = binder; ns = list(binder); ":="; tm0 = located(tm); ";"; tm1 = located(tm);
    { Surface.Let (n, ns, tm0, tm1) }
| "fun"; ns = nonempty_list(binder); "=>"; t = located(tm);
    { Surface.FunLit (ns, t) }
| if_tm

let if_tm :=
| "if"; tm0 = located(eq_tm); "then"; tm1 = located(eq_tm); "else"; tm2 = located(if_tm);
    { Surface.IfThenElse (tm0, tm1, tm2) }
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
| "("; t = tm; ")";
    { t }
| n = NAME;
    { Surface.Name n }
| "true";
    { Surface.BoolLit true }
| "false";
    { Surface.BoolLit false }
| i = NUMBER;
    { Surface.IntLit i }


let binder :=
| located(NAME)

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
