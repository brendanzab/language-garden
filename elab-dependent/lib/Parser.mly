%token UNDERSCORE "_"
%token KEYWORD_FUN "fun"
%token KEYWORD_LET "let"
%token KEYWORD_TYPE "Type"
%token <string> NAME
%token COLON ":"
%token COLON_EQUALS ":="
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"

%token LPAREN "("
%token RPAREN ")"

%token END

%start <Surface.tm> main

%%

let main :=
| t = tm; END;
    { t }

let tm :=
| "let"; p = pattern; ps = list(param); t1 = option(":"; t1 = tm; { t1 }); ":=";
    t2 = tm; ";"; t3 = tm;
    { Surface.Let (p, ps, t1, t2, t3) }
| ann_tm

let ann_tm :=
| t1 = fun_tm; ":"; t2 = ann_tm;
    { Surface.Ann (t1, t2) }
| fun_tm

let fun_tm :=
| t1 =  app_tm; "->"; t2 = fun_tm;
    { Surface.FunArrow (t1, t2) }
| "fun"; ps = nonempty_list(param); "->"; t = fun_tm;
    { Surface.FunType (ps, t) }
| "fun"; ps = nonempty_list(param); t1 = option(":"; t1 = tm; { t1 }); ":="; t2 = fun_tm;
    { Surface.FunLit (ps, t1, t2) }
| app_tm

let app_tm :=
| t = atomic_tm; ts = nonempty_list(atomic_tm);
    { Surface.FunApp (t, ts) }
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
| "("; p = pattern; ":"; t = tm; ")";
    { p, Some t }
| p = pattern;
    { p, None }

let nonempty_sequence(T) :=
| t = T; option(";");
    { [ t ] }
| t = T; ";"; ts = nonempty_sequence(T);
    { t :: ts }
