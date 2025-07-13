%token UNDERSCORE "_"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
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
| "let"; p = spanned(pattern); ":"; t1 = spanned(tm); ":="; t2 = spanned(tm); ";"; t3 = spanned(tm);
    { Surface.Let (p, t1, t2, t3) }
| t1 = spanned(app_tm); ":"; t2 = spanned(tm);
    { Surface.Ann (t1, t2) }
| t1 =  spanned(app_tm); "->"; t2 = spanned(tm);
    { Surface.Fun_arrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = spanned(tm);
    { Surface.Fun_type (ps, t) }
| "fun";  ps = nonempty_list(spanned(pattern)); "=>"; t = spanned(tm);
    { Surface.Fun_lit (ps, t) }
| app_tm

let app_tm :=
| t = spanned(atomic_tm); ts = nonempty_list(spanned(atomic_tm));
    { Surface.Fun_app (t, ts) }
| atomic_tm

let atomic_tm :=
| n = NAME;
    { Surface.Name n }
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
    { p, t }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.{ span = $loc; data } }
