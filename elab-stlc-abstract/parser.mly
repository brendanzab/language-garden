%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FUN "fun"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS_GREATER "=>"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.tm> main

%%

let main :=
| e = located(tm); END;
    { e }


(* Types *)

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.Fun_ty (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name n }


(* Terms *)

let tm :=
| "let"; n = located(NAME); ":"; ty = located(ty); ":="; tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ty, tm1, tm2) }
| "fun"; n = located(NAME); "=>"; tm = located(tm);
    { Surface.Fun_lit (n, None, tm) }
| "fun"; "("; n = located(NAME); ":"; ty = located(ty); ")"; "=>"; tm = located(tm);
    { Surface.Fun_lit (n, Some ty, tm) }
| "if"; tm1 = located(app_tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| tm = located(app_tm); ":"; ty = located(ty);
    { Surface.Ann (tm, ty) }
| app_tm

let app_tm :=
| tm1 = located(app_tm); tm2 = located(atomic_tm);
    { Surface.Fun_app (tm1, tm2) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| i = NUMBER;
    { Surface.Int_lit i }


(* Utilities *)

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
