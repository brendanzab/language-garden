%token UNDERSCORE "_"
%token FUN "fun"
%token LET "let"
%token TYPE "Type"
%token <string> NAME
%token COLON ":"
%token COLON_EQUALS ":="
%token HYPHEN_GREATER "->"
%token FULL_STOP "."
%token SEMICOLON ";"

%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"

%token END

%start <SurfaceSyntax.term> main

%%

let main :=
| t = term; END;
    { t }

let term :=
| t1 = let_term; ":"; t2 = let_term;
    { SurfaceSyntax.Ann (t1, t2) }
| let_term

let let_term :=
| "let"; p = pattern; ":"; t1 = let_term; ":="; t2 = term; ";"; t3 = let_term;
    { SurfaceSyntax.Let (p, t1, t2, t3) }
| fun_term

let fun_term :=
| t1 =  app_term; "->"; t2 = fun_term;
    { SurfaceSyntax.Arrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = fun_term;
    { List.fold_right (fun p t -> SurfaceSyntax.FunctionType (p, t)) ps t }
| "fun"; ps = nonempty_list(param); ":="; t = fun_term;
    { List.fold_right (fun p t -> SurfaceSyntax.FunctionLit (p, t)) ps t }
| app_term

let app_term :=
| t = proj_term; ts = nonempty_list(proj_term);
    { SurfaceSyntax.App (t, ts) }
| proj_term

let proj_term :=
| t = atomic_term; ls = nonempty_list("."; l = NAME; { l });
    { SurfaceSyntax.Proj (t, ls) }
| atomic_term

let atomic_term :=
| n = NAME;
    { SurfaceSyntax.Name n }
| "Type";
    { SurfaceSyntax.Type }
| "("; t = term; ")";
    { t }
| "{"; "}";
    { SurfaceSyntax.Unit }
| "{"; fs = nonempty_sequence(l = NAME; ":"; t = term; { l, t }); "}";
    { SurfaceSyntax.RecordType fs }
| "{"; fs = nonempty_sequence(l = NAME; ":="; t = term; { l, t }); "}";
    { SurfaceSyntax.RecordLit fs }

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let param :=
| "("; p = pattern; ":"; t = term; ")";
    { p, Some t }
| p = pattern;
    { p, None }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
