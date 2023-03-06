%token <string> NAME
%token <int> NUMBER
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
%token ADD "+"
%token ASTERISK "*"
%token COLON_EQUALS ":="
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
| i = NUMBER;
    { Surface.IntLit i }


let binder :=
| located(NAME)

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
