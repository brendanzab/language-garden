%token OPEN_TEMPLATE
%token CLOSE_TEMPLATE
%token <string> TEMPLATE_TEXT
%token <string> TEXT
%token <int> INT
%token <string> NAME
%token KEYWORD_ELSE "else"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token ADD "+"
%token COLON_EQUALS ":="
%token SEMI ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.template> template_main
%start <Surface.tm> tm_main

%%

let template_main :=
| t = template; END;
  { t }

let tm_main :=
| tm = tm; END;
  { tm }

let template :=
    { [] }
| s = TEMPLATE_TEXT; t = unquote_template;
    { Surface.Text s :: t }

let unquote_template :=
    { [] }
| "let"; n = NAME; ":="; tm = tm; t = template;
    { Surface.Let (n, tm) :: t }
// | "if"; tm = tm; "then"; ...; "else"; ...;
//     { Surface.IfThenElse (tm, ..., ...) :: t }
| tm = tm; t = template;
    { Surface.Term tm :: t }

let tm :=
| "let"; n = NAME; ":="; tm1 = tm; ";"; tm2 = tm;
    { Surface.Let (n, tm1, tm2) }
| "if"; tm1 = tm; "then"; tm2 = tm; "else"; tm3 = tm;
    { Surface.IfThenElse (tm1, tm2, tm3) }
| add_tm

let add_tm :=
| tm1 = atomic_tm; "+"; tm2 = add_tm;
    { Surface.Add (tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = atomic_tm; tm2 = app_tm;
    { Surface.App (tm1, tm2) }
| atomic_tm

let atomic_tm :=
| OPEN_TEMPLATE; t = template; CLOSE_TEMPLATE;
    { Surface.Template t }
| "("; tm = tm; ")";
    { tm }
| s = TEXT;
    { Surface.TextLit s }
| n = NAME;
    { Surface.Name n }
| i = INT;
    { Surface.IntLit i }
