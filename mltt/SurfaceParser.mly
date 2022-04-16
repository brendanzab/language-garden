%token UNDERSCORE "_"
%token FUN "fun"
%token LET "let"
%token TYPE "Type"
%token <string> NAME
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS_GREATER "=>"
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
| "fun"; "("; p = pattern; ":"; t1 = term; ")"; "->"; t2 = fun_term;
    { SurfaceSyntax.FunctionType (p, t1, t2) }
| "fun"; "("; p = pattern; ":"; t1 = term; ")"; "=>"; t2 = fun_term;
    { SurfaceSyntax.FunctionLit (p, Some t1, t2) }
| "fun"; p = pattern; "=>"; t2 = fun_term;
    { SurfaceSyntax.FunctionLit (p, None, t2) }
| app_term

let app_term :=
| t = atomic_term; ts = nonempty_list(atomic_term);
    { SurfaceSyntax.App (t, ts) }
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
| t = atomic_term; "."; l = NAME;
    { SurfaceSyntax.Proj (t, l) }

let pattern :=
| "_";
    { None }
| n = NAME;
    { Some n }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
