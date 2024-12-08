%token <string> NAME
%token <int> NUMBER
%token KEYWORD_BOOL "Bool"
%token KEYWORD_FALSE "false"
%token KEYWORD_FUN "fun"
%token KEYWORD_INT "Int"
%token KEYWORD_LET "let"
%token KEYWORD_TRUE "true"
%token ADD "+"
%token ASTERISK "*"
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS_GREATER "=>"
%token HYPHEN "-"
%token HYPHEN_GREATER "->"
%token SEMICOLON ";"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%{

  (** Returns the index of the given element in the list *)
  let elem_index a =
    let rec go i = function
      | [] -> None
      | x :: xs -> if x = a then Some i else go (i + 1) xs in
    go 0

%}

%start <Lang_Fun.tm> main

(* The name environment is used to assign De Bruijn indices during parsing. *)
%type <string list -> Lang_Fun.tm> tm

%%

let main :=
| e = tm; END;
    { e [] }

let ty :=
| t1 = atomic_ty; "->"; t2 = ty;
    { Lang_Fun.FunType (t1, t2) }
| atomic_ty

let atomic_ty :=
| "Bool";
    { Lang_Fun.BoolType }
| "Int";
    { Lang_Fun.IntType }
| "("; t = ty; ")";
    { t }

let tm :=
| "let"; n = NAME; ":"; t = ty; ":="; e1 = tm; ";"; e2 = tm;
    { fun names -> Lang_Fun.Let (n, t, e1 names, e2 (n :: names)) }
| "fun"; "("; n = NAME; ":"; t = ty; ")"; "=>"; e = tm;
    { fun names -> Lang_Fun.FunLit (n, t, e (n :: names)) }
| add_tm

let add_tm :=
| e1 = mul_tm; "+"; e2 = add_tm;
    { fun names -> Lang_Fun.PrimApp (`Add, [e1 names; e2 names]) }
| e1 = mul_tm; "-"; e2 = add_tm;
    { fun names -> Lang_Fun.PrimApp (`Sub, [e1 names; e2 names]) }
| mul_tm

let mul_tm :=
| e1 = app_tm; "*"; e2 = mul_tm;
    { fun names -> Lang_Fun.PrimApp (`Mul, [e1 names; e2 names]) }
| app_tm

let app_tm :=
| e1 = app_tm; e2 = atomic_tm;
    { fun names -> Lang_Fun.FunApp (e1 names, e2 names) }
| "-"; e = atomic_tm;
    { fun names -> Lang_Fun.PrimApp (`Neg, [e names]) }
| atomic_tm

let atomic_tm :=
| "("; e = tm; ")";
    { e }
| n = NAME;
    { fun names ->
        (* Assign a de Bruijn index for the variable *)
        match elem_index n names with
        | Some n -> Lang_Fun.Var n
        | None -> raise (Lang_Fun.UnboundName n)
    }
| i = NUMBER;
    { fun _ -> Lang_Fun.IntLit i }
| "true";
    { fun _ -> Lang_Fun.BoolLit true }
| "false";
    { fun _ -> Lang_Fun.BoolLit false }
