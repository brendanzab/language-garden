%token <string> NAME
%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token ADD "+"
%token ASTERISK "*"
%token COLON_EQUALS ":="
%token EQUALS "="
%token FORWARD_SLASH "/"
%token HYPHEN "-"
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

%start <Tree_lang.expr> main

(* The name environment is used to assign De Bruijn indices during parsing. *)
%type <string list -> Tree_lang.expr> expr

%%

let main :=
| e = expr; END;
    { e [] }

let expr :=
| "let"; n = NAME; ":="; e1 = expr; ";"; e2 = expr;
    { fun names -> Tree_lang.let_ n (e1 names) (e2 (n :: names)) }
| "if"; e1 = eq_expr; "then"; e2 = eq_expr; "else"; e3 = expr;
    { fun names -> Tree_lang.if_then_else (e1 names) (e2 names) (e3 names) }
| eq_expr

let eq_expr :=
| e1 = add_expr; "="; e2 = eq_expr;
    { fun names -> Tree_lang.eq (e1 names) (e2 names) }
| add_expr

let add_expr :=
| e1 = mul_expr; "+"; e2 = add_expr;
    { fun names -> Tree_lang.add (e1 names) (e2 names) }
| e1 = mul_expr; "-"; e2 = add_expr;
    { fun names -> Tree_lang.sub (e1 names) (e2 names) }
| mul_expr

let mul_expr :=
| e1 = atomic_expr; "*"; e2 = mul_expr;
    { fun names -> Tree_lang.mul (e1 names) (e2 names) }
| e1 = atomic_expr; "/"; e2 = mul_expr;
    { fun names -> Tree_lang.div (e1 names) (e2 names) }
| atomic_expr

let atomic_expr :=
| "("; e = expr; ")";
    { e }
| n = NAME;
    { fun names ->
        (* Assign a De Bruijn index for the variable *)
        match elem_index n names with
        | Some n -> Tree_lang.var n
        | None -> raise (Tree_lang.Unbound_name ($loc(n), n))
    }
| i = NUMBER;
    { fun _ -> Tree_lang.int i }
| "true";
    { fun _ -> Tree_lang.bool true }
| "false";
    { fun _ -> Tree_lang.bool false }
| "-"; e = atomic_expr;
    { fun names -> Tree_lang.neg (e names) }
