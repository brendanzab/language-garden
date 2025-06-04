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
| t = located(tm); END;
    { t }

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }

let tm :=
| "let"; p = located(pattern); ps = list(param); t1 = option(":"; t1 = located(tm); { t1 }); ":=";
    t2 = located(tm); ";"; t3 = located(tm);
    { Surface.Let (p, ps, t1, t2, t3) }
| t1 = located(app_tm); ":"; t2 = located(tm);
    { Surface.Ann (t1, t2) }
| t1 =  located(app_tm); "->"; t2 = located(tm);
    { Surface.Fun_arrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = located(tm);
    { Surface.Fun_type (ps, t) }
| "fun"; ps = nonempty_list(param); t1 = option(":"; t1 = located(tm); { t1 }); "=>"; t2 = located(tm);
    { Surface.Fun_lit (ps, t1, t2) }
| app_tm

let app_tm :=
| t = located(proj_tm); ts = nonempty_list(located(proj_tm));
    { Surface.App (t, ts) }
| t1 = located(app_tm); "["; "="; t2 = located(tm); "]";
    { Surface.Sing_type (t1, t2) }
| t = located(app_tm); "["; ps = nonempty_sequence(l = NAME; ":="; t = located(tm); { l, t }); "]";
    { Surface.Patch (t, ps) }
| proj_tm

let proj_tm :=
| t = located(atomic_tm); ls = nonempty_list("."; l = located(NAME); { l });
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
| "{"; ds = nonempty_sequence(l = located(NAME); ":"; t = located(tm); { l, t }); "}";
    { Surface.Rec_type ds }
| "{"; ds = nonempty_sequence(defn); "}";
    { Surface.Rec_lit ds }

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| "("; p = located(pattern); ":"; t = located(tm); ")";
    { p, Some t }
| p = located(pattern);
    { p, None }

let defn :=
| l = located(NAME);
    { l, None }
| l = located(NAME); ps = list(param); ":="; t = located(tm);
    { l, Some (ps, t) }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
