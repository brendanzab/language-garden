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
| t = spanned(atomic_tm); ts = nonempty_list(spanned(atomic_tm));
    { Surface.Fun_app (t, ts) }
| atomic_tm

let atomic_tm :=
| n = NAME;
    { Surface.Name n }
| "Type";
    { Surface.Univ }
| "("; t = tm; ")";
    { t }


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


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.{ span = $loc; data } }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
