%token OPEN_TERM
%token CLOSE_TERM
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
%token COLON ":"
%token COLON_EQUALS ":="
%token HYPHEN_GREATER "->"
%token SEMI ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.template> template_main
%start <Surface.tm> tm_main
%type <Surface.ty_data> ty

%%

let tm_main :=
| tm = located(tm); END;
  { tm }

let template_main :=
| t = template; END;
  { t }


(* Types *)

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.Fun (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
  { Surface.Name n }


(* Terms *)

let tm :=
| "let"; n = located(NAME); ps = list(param); ty = ioption(":"; ~ = located(ty); <>); ":="; tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ps, ty, tm1, tm2) }
| "if"; tm1 = located(tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.IfThenElse (tm1, tm2, tm3) }
| tm = located(add_tm); ":"; ty = located(ty);
    { Surface.Ann (tm, ty) }
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


(* Templates *)

let template :=
    { [] }
| f = located(text_fragment); t = unquote_template;
    { f :: t }

let unquote_template :=
    { [] }
| OPEN_TERM; f = located(unquote_fragment); CLOSE_TERM; t = template;
    { f :: t }

let text_fragment :=
| s = TEMPLATE_TEXT;
    { Surface.TextFragment s }

let unquote_fragment :=
| "let"; n = located(NAME); ps = list(param); ty = ioption(":"; ~ = located(ty); <>); ":="; tm = located(tm);
    { Surface.LetFragment (n, ps, ty, tm) }
| tm = located(tm);
    { Surface.TermFragment tm }


(* Utilities *)

let binder :=
| located(NAME)

let param :=
| n = binder;
    { n, None }
| "("; n = binder; ":"; ty = located(ty); ")";
    { n, Some ty }

let located(X) :=
| data = X;
    { Surface.{ loc = $loc; data } }
