%token UNDERSCORE "_"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
%token KEYWORD_TYPE "Type"
%token <string> NAME
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS_GREATER "=>"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"

%token LPAREN "("
%token RPAREN ")"

%token END

%start <Surface.tm> main

%%

let main :=
| t = tm; END;
    { t }

let tm :=
| "let"; p = pattern; ":"; t1 = tm; ":="; t2 = tm; ";"; t3 = tm;
    { Surface.Let (p, t1, t2, t3) }
| t1 = app_tm; ":"; t2 = tm;
    { Surface.Ann (t1, t2) }
| t1 =  app_tm; "->"; t2 = tm;
    { Surface.FunArrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = tm;
    { Surface.FunType (ps, t) }
| "fun";  ps = nonempty_list(pattern); "=>"; t = tm;
    { Surface.FunLit (ps, t) }
| app_tm

let app_tm :=
| t = atomic_tm; ts = nonempty_list(atomic_tm);
    { Surface.FunApp (t, ts) }
| atomic_tm

let atomic_tm :=
| n = NAME;
    { Surface.Name n }
| "Type";
    { Surface.Univ }
| "("; t = tm; ")";
    { t }

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| "("; p = pattern; ":"; t = tm; ")";
    { p, t }
