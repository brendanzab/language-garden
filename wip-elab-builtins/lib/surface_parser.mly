%token KEYWORD_DEF "def"
%token KEYWORD_USE "use"
%token <string> NAME
%token <string> NUMBER
%token COLON ":"
%token COLON_EQUALS ":="
%token FULL_STOP "."
%token HYPHEN "-"
%token PLUS "+"
%token SEMICOLON ";"

%token LPAREN "("
%token RPAREN ")"

%token END

%start <Surface_syntax.item list> main

%%

let main :=
| is = list(item); END;
    { is }

let item :=
| "use"; path = separated_nonempty_list(".", NAME); ";";
    { Surface_syntax.Use { path } }
| "def"; label = NAME; ty = option(":"; t = tm; { t }); ":="; tm = tm; ";";
    { Surface_syntax.Def { label; ty; tm } }

let tm :=
| lhs = atomic_tm; "+"; rhs = tm;
    { Surface_syntax.Binop (lhs, Surface_syntax.Add, rhs) }
| lhs = atomic_tm; "-"; rhs = tm;
    { Surface_syntax.Binop (lhs, Surface_syntax.Sub, rhs) }
| t = atomic_tm;
    { t }

let atomic_tm :=
| "("; t = tm; ")";
    { t }
| path = separated_nonempty_list(".", NAME);
    { Surface_syntax.Path path }
| num = NUMBER;
    { Surface_syntax.NumLit num }
