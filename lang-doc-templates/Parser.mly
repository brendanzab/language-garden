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
| tm = located(tm); END;
  { tm }

let template :=
    { [] }
| f = located(text_fragment); t = unquote_template;
    { f :: t }

let unquote_template :=
    { [] }
| f = located(unquote_fragment); t = template;
    { f :: t }

let text_fragment :=
| s = TEMPLATE_TEXT;
    { Surface.TextFragment s }

let unquote_fragment :=
| "let"; n = located(NAME); ":="; tm = located(tm);
    { Surface.LetFragment (n, tm) }
| tm = located(tm);
    { Surface.TermFragment tm }

let tm :=
| "let"; n = located(NAME); ":="; tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, tm1, tm2) }
| "if"; tm1 = located(tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.IfThenElse (tm1, tm2, tm3) }
| add_tm

let add_tm :=
| tm1 = located(atomic_tm); "+"; tm2 = located(add_tm);
    { Surface.Add (tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = located(atomic_tm); tm2 = located(app_tm);
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

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
