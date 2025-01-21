%token UNDERSCORE "_"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
%token KEYWORD_TYPE "Type"
%token <string> NAME
%token COLON ":"
%token COLON_EQUALS ":="
%token DOT "."
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"

%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token LPAREN "("
%token RPAREN ")"

%token END

%start <Surface.tm> main

%%

let main :=
| t = tm; END;
    { t }

let tm :=
| "let"; p = pattern; t1 = option(":"; t1 = tm; { t1 }); ":="; t2 = tm; ";"; t3 = tm;
    { Surface.Let (p, t1, t2, t3) }
| t1 = app_tm; ":"; t2 = tm;
    { Surface.Ann (t1, t2) }
| t1 =  app_tm; "->"; t2 = tm;
    { Surface.Fun_arrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = tm;
    { Surface.Fun_type (ps, t) }
| "fun"; ps = nonempty_list(pattern); "=>"; t = tm;
    { Surface.Fun_lit (ps, t) }
| app_tm

let app_tm :=
| t = proj_tm; ts = nonempty_list(proj_tm);
    { Surface.App (t, ts) }
| t1 = app_tm; "["; "="; t2 = tm; "]";
    { Surface.Sing_type (t1, t2) }
| t = app_tm; "["; ps = nonempty_sequence(l = NAME; ":="; t = tm; { l, t }); "]";
    { Surface.Patch (t, ps) }
| proj_tm

let proj_tm :=
| t = atomic_tm; ls = nonempty_list("."; l = NAME; { l });
    { Surface.Proj (t, ls) }
| atomic_tm

let atomic_tm :=
| "("; t = tm; ")";
    { t }
| n = NAME;
    { Surface.Name n }
| "Type";
    { Surface.Univ }
| "{"; "}";
    { Surface.Rec_unit }
| "{"; ds = nonempty_sequence(l = NAME; ":"; t = tm; { l, t }); "}";
    { Surface.Rec_type ds }
| "{"; ds = nonempty_sequence(defn); "}";
    { Surface.Rec_lit ds }

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| "("; p = pattern; ":"; t = tm; ")";
    { p, t }

let defn :=
| l = NAME;
    { l, None }
| l = NAME; ":="; t = tm;
    { l, Some t }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
