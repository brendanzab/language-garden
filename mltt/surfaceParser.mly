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

%start <Surface.term> main

%%

let main :=
| t = term; END;
    { t }

let term :=
| t1 = let_term; ":"; t2 = let_term;
    { Surface.Ann (t1, t2) }
| let_term

let let_term :=
| "let"; p = pattern; ":"; t1 = let_term; ":="; t2 = let_term; ";"; t3 = let_term;
    { Surface.Let (p, t1, t2, t3) }
| app_term

let app_term :=
| "fun"; "("; p = pattern; ":"; t1 = term; ")"; "->"; t2 = app_term;
    { Surface.FunctionType (p, t1, t2) }
| "fun"; "("; p = pattern; ":"; t1 = term; ")"; "=>"; t2 = app_term;
    { Surface.FunctionLit (p, Some t1, t2) }
| "fun"; p = pattern; "=>"; t2 = app_term;
    { Surface.FunctionLit (p, None, t2) }
| t = atomic_term; ts = nonempty_list(atomic_term);
    { Surface.App (t, ts) }
| atomic_term

let atomic_term :=
| n = NAME;
    { Surface.Name n }
| "Type";
    { Surface.Type }
| "("; t = term; ")";
    { t }
| "{"; "}";
    { Surface.Unit }
| "{"; fs = nonempty_sequence(l = NAME; ":"; t = term; { l, t }); "}";
    { Surface.RecordType fs }
| "{"; fs = nonempty_sequence(l = NAME; ":="; t = term; { l, t }); "}";
    { Surface.RecordLit fs }
| t = atomic_term; "."; l = NAME;
    { Surface.Proj (t, l) }

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
