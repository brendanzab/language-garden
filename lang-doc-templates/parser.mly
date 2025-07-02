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
%token COMMA ","
%token HYPHEN_GREATER "->"
%token SEMI ";"
%token OPEN_BRACKET "["
%token CLOSE_BRACKET "]"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.template> template_main
%start <Surface.tm> tm_main
%type <Surface.ty_data> ty

%%

let tm_main :=
| tm = spanned(tm); END;
  { tm }

let template_main :=
| t = template; END;
  { t }


(* Types *)

let ty :=
| ty1 = spanned(app_ty); "->"; ty2 = spanned(ty);
    { Surface.Fun_ty (ty1, ty2) }
| app_ty

let app_ty :=
| n = NAME; tys = nonempty_list(spanned(atomic_ty));
    { Surface.Name (n, tys) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| n = NAME;
    { Surface.Name (n, []) }


(* Terms *)

let tm :=
| "let"; n = spanned(NAME); ps = list(param); ty = ioption(":"; ~ = spanned(ty); <>); ":="; tm1 = spanned(tm); ";"; tm2 = spanned(tm);
    { Surface.Let (n, ps, ty, tm1, tm2) }
| "if"; tm1 = spanned(tm); "then"; tm2 = spanned(tm); "else"; tm3 = spanned(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| tm = spanned(add_tm); ":"; ty = spanned(ty);
    { Surface.Ann (tm, ty) }
| add_tm

let add_tm :=
| tm1 = spanned(atomic_tm); "+"; tm2 = spanned(add_tm);
    { Surface.Infix (`Add, tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = spanned(atomic_tm); tm2 = spanned(app_tm);
    { Surface.App (tm1, tm2) }
| atomic_tm

let atomic_tm :=
| OPEN_TEMPLATE; t = template; CLOSE_TEMPLATE;
    { Surface.Template_lit t }
| "("; tm = tm; ")";
    { tm }
| "["; tms = trailing_list(",", spanned(tm));  "]";
    { Surface.List_lit tms }
| s = TEXT;
    { Surface.Text_lit s }
| n = NAME;
    { Surface.Name n }
| i = INT;
    { Surface.Int_lit i }


(* Templates *)

let template :=
    { [] }
| f = spanned(text_fragment); t = unquote_template;
    { f :: t }

let unquote_template :=
    { [] }
| OPEN_TERM; f = spanned(unquote_fragment); CLOSE_TERM; t = template;
    { f :: t }

let text_fragment :=
| s = TEMPLATE_TEXT;
    { Surface.Text_fragment s }

let unquote_fragment :=
| "let"; n = spanned(NAME); ps = list(param); ty = ioption(":"; ~ = spanned(ty); <>); ":="; tm = spanned(tm);
    { Surface.Let_fragment (n, ps, ty, tm) }
| tm = spanned(tm);
    { Surface.Term_fragment tm }


(* Binders *)

let binder :=
| spanned(NAME)

let param :=
| n = binder;
    { n, None }
| "("; n = binder; ":"; ty = spanned(ty); ")";
    { n, Some ty }


(* Utilities *)

let spanned(X) :=
| data = X;
    { Surface.{ span = $loc; data } }

let trailing_list(Sep, T) :=
    { [] }
| trailing_nonempty_list(Sep, T)

let trailing_nonempty_list(Sep, T) :=
| t = T; option(Sep);
    { [ t ] }
| t = T; Sep; ts = trailing_nonempty_list(Sep, T);
    { t :: ts }
