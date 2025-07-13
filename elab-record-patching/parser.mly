%token UNDERSCORE "_"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
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
| t = spanned(tm); END;
    { t }


(* Terms *)

let tm :=
| "let"; p = spanned(pattern); ps = list(param); t1 = option(":"; t1 = spanned(tm); { t1 }); ":=";
    t2 = spanned(tm); ";"; t3 = spanned(tm);
    { Surface.Let (p, ps, t1, t2, t3) }
| t1 = spanned(app_tm); ":"; t2 = spanned(tm);
    { Surface.Ann (t1, t2) }
| t1 =  spanned(app_tm); "->"; t2 = spanned(tm);
    { Surface.Fun_arrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = spanned(tm);
    { Surface.Fun_type (ps, t) }
| "fun"; ps = nonempty_list(param); t1 = option(":"; t1 = spanned(tm); { t1 }); "=>"; t2 = spanned(tm);
    { Surface.Fun_lit (ps, t1, t2) }
| app_tm

let app_tm :=
| t = spanned(proj_tm); ts = nonempty_list(spanned(proj_tm));
    { Surface.App (t, ts) }
| t1 = spanned(app_tm); "["; "="; t2 = spanned(tm); "]";
    { Surface.Sing_type (t1, t2) }
| t = spanned(app_tm); "["; ps = nonempty_sequence(l = NAME; ":="; t = spanned(tm); { l, t }); "]";
    { Surface.Patch (t, ps) }
| proj_tm

let proj_tm :=
| t = spanned(atomic_tm); ls = nonempty_list("."; l = spanned(NAME); { l });
    { Surface.Proj (t, ls) }
| atomic_tm

let atomic_tm :=
| "("; t = tm; ")";
    { t }
| n = NAME;
    { Surface.Name n }
| "{"; "}";
    { Surface.Rec_unit }
| "{"; ds = nonempty_sequence(l = spanned(NAME); ":"; t = spanned(tm); { l, t }); "}";
    { Surface.Rec_type ds }
| "{"; ds = nonempty_sequence(defn); "}";
    { Surface.Rec_lit ds }


(* Binders *)

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| "("; p = spanned(pattern); ":"; t = spanned(tm); ")";
    { p, Some t }
| p = spanned(pattern);
    { p, None }

let defn :=
| l = spanned(NAME);
    { l, None }
| l = spanned(NAME); ps = list(param); ":="; t = spanned(tm);
    { l, Some (ps, t) }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.{ span = $loc; data } }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
