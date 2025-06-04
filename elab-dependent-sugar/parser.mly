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
| t = located(atomic_tm); ts = nonempty_list(located(atomic_tm));
    { Surface.Fun_app (t, ts) }
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
| "("; p = located(pattern); ":"; t = located(tm); ")";
    { p, Some t }
| p = located(pattern);
    { p, None }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
