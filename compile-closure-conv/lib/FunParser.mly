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

%start <FunLang.tm> main

(* The name environment is used to assign De Bruijn indices during parsing. *)
%type <string list -> FunLang.tm> tm

%%

let main :=
| e = tm; END;
    { e [] }

let ty :=
| t1 = atomic_ty; "->"; t2 = ty;
    { FunLang.FunType (t1, t2) }
| atomic_ty

let atomic_ty :=
| "Bool";
    { FunLang.BoolType }
| "Int";
    { FunLang.IntType }
| "("; t = ty; ")";
    { t }

let tm :=
| "let"; n = NAME; ":"; t = ty; ":="; e1 = tm; ";"; e2 = tm;
    { fun names -> FunLang.Let (n, t, e1 names, e2 (n :: names)) }
| "fun"; "("; n = NAME; ":"; t = ty; ")"; "=>"; e = tm;
    { fun names -> FunLang.FunLit (n, t, e (n :: names)) }
| add_tm

let add_tm :=
| e1 = mul_tm; "+"; e2 = add_tm;
    { fun names -> FunLang.PrimApp (`Add, [e1 names; e2 names]) }
| e1 = mul_tm; "-"; e2 = add_tm;
    { fun names -> FunLang.PrimApp (`Sub, [e1 names; e2 names]) }
| mul_tm

let mul_tm :=
| e1 = app_tm; "*"; e2 = mul_tm;
    { fun names -> FunLang.PrimApp (`Mul, [e1 names; e2 names]) }
| app_tm

let app_tm :=
| e1 = app_tm; e2 = atomic_tm;
    { fun names -> FunLang.FunApp (e1 names, e2 names) }
| "-"; e = atomic_tm;
    { fun names -> FunLang.PrimApp (`Neg, [e names]) }
| atomic_tm

let atomic_tm :=
| "("; e = tm; ")";
    { e }
| n = NAME;
    { fun names ->
        (* Assign a de Bruijn index for the variable *)
        match elem_index n names with
        | Some n -> FunLang.Var n
        | None -> raise (FunLang.UnboundName n)
    }
| i = NUMBER;
    { fun _ -> FunLang.IntLit i }
| "true";
    { fun _ -> FunLang.BoolLit true }
| "false";
    { fun _ -> FunLang.BoolLit false }
