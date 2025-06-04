%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_END "end"
%token KEYWORD_FUN "fun"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_MATCH "match"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token KEYWORD_WITH "with"
%token ADD "+"
%token ASTERISK "*"
%token COLON ":"
%token COLON_EQUALS ":="
%token DOT "."
%token EQUALS "="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"
%token UNDERSCORE "_"
%token VERTICAL_LINE "|"
%token OPEN_BRACE "["
%token CLOSE_BRACE "]"
%token OPEN_BRACKET "{"
%token CLOSE_BRACKET "}"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Surface.tm> main

%%

let main :=
| tm = located(tm); END;
    { tm }


(* Types *)

let ty :=
| ty1 = located(atomic_ty); "->"; ty2 = located(ty);
    { Surface.Fun_type (ty1, ty2) }
| atomic_ty

let atomic_ty :=
| "("; ty = ty; ")";
    { ty }
| "{"; fs = trailing_list(";", ~ = located(NAME); ":"; ~ = located(ty); <>); "}";
    { Surface.Record_type fs  }
| "["; "|"; "]";
    { Surface.Variant_type [] }
| "["; option("|"); r = separated_nonempty_list("|", l = located(NAME); ":"; ty = located(ty); { l, ty }); "]";
    { Surface.Variant_type r }
| n = NAME;
    { Surface.Name n }
| UNDERSCORE;
    { Surface.Placeholder }


(* Terms *)

let tm :=
| "let"; n = located(binder); ps = list(param); ty = option(":"; ty = located(ty); { ty }); ":=";
    tm1 = located(tm); ";"; tm2 = located(tm);
    { Surface.Let (n, ps, ty, tm1, tm2) }
| "fun"; ps = nonempty_list(param); "=>"; t = located(tm);
    { Surface.Fun_lit (ps, t) }
| "if"; tm1 = located(eq_tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
    { Surface.If_then_else (tm1, tm2, tm3) }
| "match"; tm1 = located(eq_tm); "with"; option("|"); clauses = separated_list("|", clause); "end";
    { Surface.Match (tm1, clauses) }
| tm = located(eq_tm); ":"; ty = located(ty);
    { Surface.Ann (tm, ty) }
| eq_tm

let eq_tm :=
| tm1 = located(add_tm); "="; tm2 = located(eq_tm);
    { Surface.Op2 (`Eq, tm1, tm2) }
| add_tm

let add_tm :=
| tm1 = located(mul_tm); "+"; tm2 = located(add_tm);
    { Surface.Op2 (`Add, tm1, tm2) }
| tm1 = located(mul_tm); "-"; tm2 = located(add_tm);
    { Surface.Op2 (`Sub, tm1, tm2) }
| mul_tm

let mul_tm :=
| tm1 = located(app_tm); "*"; tm2 = located(mul_tm);
    { Surface.Op2 (`Mul, tm1, tm2) }
| app_tm

let app_tm :=
| tm1 = located(app_tm); tm2 = located(proj_tm);
    { Surface.App (tm1, tm2) }
| "-"; tm = located(proj_tm);
    { Surface.Op1 (`Neg, tm) }
| proj_tm

let proj_tm :=
| tm = located(proj_tm); "."; l = located(NAME);
    { Surface.Proj (tm, l) }
| atomic_tm

let atomic_tm :=
| "("; tm = tm; ")";
    { tm }
| n = NAME;
    { Surface.Name n }
| "{"; fs = trailing_list(";", ~ = located(NAME); ":="; ~ = located(tm); <>); "}";
    { Surface.Record_lit fs  }
| "["; l = located(NAME); ":="; tm = located(tm); "]";
    { Surface.Variant_lit (l, tm) }
| "true";
    { Surface.Bool_lit true }
| "false";
    { Surface.Bool_lit false }
| i = NUMBER;
    { Surface.Int_lit i }


(* Binders *)

let binder :=
| n = NAME;
    { Some n }
| "_";
    { None }

let param :=
| n = located(binder);
    { n, None }
| "("; n = located(binder); ":"; ty = located(ty); ")";
    { n, Some ty }

let pattern :=
| "["; l = located(NAME); ":="; n = located(binder); "]";
    { Surface.Variant_lit (l, n) : Surface.pattern_data }

let clause :=
| p = located(pattern); "=>"; tm = located(tm); { p, tm }


(* Utilities *)

let located(X) :=
  | data = X;
      { Surface.{ loc = $loc; data } }

let trailing_list(Sep, T) :=
  | { [] }
  | trailing_nonempty_list(Sep, T)

let trailing_nonempty_list(Sep, T) :=
  | t = T; option(Sep);
      { [ t ] }
  | t = T; Sep; ts = trailing_nonempty_list(Sep, T);
      { t :: ts }
