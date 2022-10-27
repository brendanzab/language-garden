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

%start <SurfaceSyntax.item list> main

%%

let main :=
| is = list(item); END;
    { is }

let item :=
| "use"; path = separated_nonempty_list(".", NAME); ";";
    { SurfaceSyntax.Use { path } }
| "def"; label = NAME; ty = option(":"; t = tm; { t }); ":="; tm = tm; ";";
    { SurfaceSyntax.Def { label; ty; tm } }

let tm :=
| lhs = atomic_tm; "+"; rhs = tm;
    { SurfaceSyntax.Binop (lhs, SurfaceSyntax.Add, rhs) }
| lhs = atomic_tm; "-"; rhs = tm;
    { SurfaceSyntax.Binop (lhs, SurfaceSyntax.Sub, rhs) }
| t = atomic_tm;
    { t }

let atomic_tm :=
| "("; t = tm; ")";
    { t }
| path = separated_nonempty_list(".", NAME);
    { SurfaceSyntax.Path path }
| num = NUMBER;
    { SurfaceSyntax.NumLit num }
