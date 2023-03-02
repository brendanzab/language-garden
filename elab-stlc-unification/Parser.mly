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
| tm = tm; END;
    { tm }

let tm :=
| "let"; n = NAME; ":="; tm0 = tm; ";"; tm1 = tm;
    { Surface.Let (n, tm0, tm1) }
| "fun"; n = NAME; "=>"; t = tm;
    { Surface.FunLit (n, t) }
| add_tm

let add_tm :=
| tm0 = mul_tm; "+"; tm1 = add_tm;
    { Surface.Op2 (`Add, tm0, tm1) }
| tm0 = mul_tm; "-"; tm1 = add_tm;
    { Surface.Op2 (`Sub, tm0, tm1) }
| mul_tm

let mul_tm :=
| tm0 = app_tm; "*"; tm1 = mul_tm;
    { Surface.Op2 (`Mul, tm0, tm1) }
| app_tm

let app_tm :=
| tm0 = app_tm; tm1 = atomic_tm;
    { Surface.FunApp (tm0, tm1) }
| "-"; tm = atomic_tm;
    { Surface.Op1 (`Neg, tm) }
| atomic_tm

let atomic_tm :=
| "("; t = tm; ")";
    { t }
| n = NAME;
    { Surface.Name n }
| i = NUMBER;
    { Surface.IntLit i }
