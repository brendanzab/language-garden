%token <string> NAME
%token KEYWORD_A "A"
%token KEYWORD_B "B"
%token KEYWORD_C "C"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
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

let ty :=
| ty1 = atomic_ty; "->"; ty2 = ty;
    { Surface.FunTy (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| "A";
    { Surface.A }
| "B";
    { Surface.B }
| "C";
    { Surface.C }

let tm :=
| "let"; n = located(NAME); ":"; ty = ty; ":="; tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ty, tm1, tm2) }
| "fun"; n = located(NAME); "=>"; tm = located(tm);
    { Surface.FunLit (n, None, tm) }
| "fun"; "("; n = located(NAME); ":"; ty = ty; ")"; "=>"; tm = located(tm);
    { Surface.FunLit (n, Some ty, tm) }
| tm = located(app_tm); ":"; ty = ty;
    { Surface.Ann (tm, ty) }
| app_tm

let app_tm :=
| tm1 = located(app_tm); tm2 = located(atomic_tm);
    { Surface.FunApp (tm1, tm2) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Var n }

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
