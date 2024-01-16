[@@@warning "-duplicate-definitions"]

type term =
  | Name of string
  | Let of string * term * term
  | Add of term * term
  | Template of template
  | IfThenElse of term * term * term
  | TextLit of string
  | IntLit of int

and template =
  fragment list

and fragment =
  | Text of string
  | Term of term
  | Let of string * term
  | IfThenElse of term * template * template

type context =
  (string * Core.ty) list

let rec elab_check (context : context) (term : term) (ty : Core.ty) : Core.tm =
  match term with
  | IfThenElse (term1, term2, term3) ->
      BoolElim (
        elab_check context term1 Bool,
        elab_check context term2 ty,
        elab_check context term3 ty)
  | term ->
      let term, ty' = elab_synth context term in
      if ty = ty' then term else
        failwith "type mismatch"

and elab_synth (context : context) (term : term) : Core.tm * Core.ty =
  match term with
  | Name name -> Var name, List.assoc name context
  | Let (name, def_term, body_term) ->
      let def_term, def_ty = elab_synth context def_term in
      let body_term, body_ty = elab_synth ((name, def_ty) :: context) body_term in
      Let (name, def_ty, def_term, body_term), body_ty
  | Template template -> elab_template context template, Text
  | Add (term1, term2) ->
      TextConcat (elab_check context term1 Text, elab_check context term2 Text), Text
  | IfThenElse (_, _, _) ->
      failwith "ambiguous if expression"
  | TextLit s -> TextLit s, Text
  | IntLit n -> IntLit n, Int

and[@tail_mod_cons] elab_template (context : context) (template : template) : Core.tm =
  match template with
  | [] -> TextLit ""
  | Text s :: template ->
      TextConcat (TextLit s, (elab_template [@tailcall]) context template)
  | Term term :: template ->
      TextConcat (elab_check context term Text, (elab_template [@tailcall]) context template)
  | Let (name, def_term) :: template ->
      let def_term, def_ty = elab_synth context def_term in
      Let (name, def_ty, def_term, (elab_template [@tailcall]) ((name, def_ty) :: context) template)
  | IfThenElse (term, template1, template2) :: template ->
      TextConcat (
        BoolElim (
          elab_check context term Bool,
          elab_template context template1,
          elab_template context template2),
        (elab_template [@tailcall]) context template)

module Parser : sig

  val parse_term : string -> (term, string) result
  val parse_template : string -> (template, string) result

end = struct

  open Angstrom

  let string_of_char_list (s : char list) : string =
    s |> List.to_seq |> String.of_seq (* FIXME: slooow! *)

  (* Lexical syntax *)

  let is_digit c = '0' <= c && c <= '9'
  let is_lower_case c = 'a' <= c && c <= 'z'
  let is_upper_case c = 'A' <= c && c <= 'Z'
  let is_ident_start c = c = '_' || is_lower_case c || is_upper_case c
  let is_ident_continue c = is_ident_start c || is_digit c

  let keywords = ["let"; "if"; "then"; "else"]
  let whitespace = skip_while (function ' ' | '\n' | '\t' -> true | _ -> false)
  let lexeme p = p <* whitespace

  let token s : unit t =
    lexeme (string s <?> "expected " ^ s) *> return ()

  let name : string t =
    let* i = lexeme (
      let+ first = satisfy is_ident_start
      and+ rest = take_while is_ident_continue
      in String.make 1 first ^ rest)
    in
    if List.mem i keywords then
      fail (i ^ " is a keyword")
    else
      return i

  let text_lit : string t =
    string "\"" *> many_till any_char (string "\"")
    >>| string_of_char_list

  let int_lit : int t =
    let* s = lexeme (take_while1 is_digit) in
    match int_of_string_opt s with
    | Some i -> return i
    | None -> fail "expected int literal"

  (* Concrete syntax *)

  let term : term t = fix (fun term ->
    let let_term =
      let+ n = token "let" *> name <* token ":="
      and+ tm1 = term <* token ";"
      and+ tm2 = term
      in Let (n, tm1, tm2)
    in

    let if_then_else_term =
      let+ tm1 = token "if" *> term
      and+ tm2 = token "then" *> term
      and+ tm3 = token "else" *> term
      in IfThenElse (tm1, tm2, tm3)
    in

    let add_term =
      let+ tm1 = term <* token "+"
      and+ tm2 = term
      in Add (tm1, tm2)
    in

    let template_term =
      (* FIXME: use `%` instead? *)
      let+ t = string "$\"" *> fail "TODO" in (* TODO: template *)
      Template t
    in

    let text_lit_term =
      let+ s = text_lit in TextLit s
    in

    let int_lit_term =
      let+ i = int_lit in IntLit i
    in

    let name_term =
      let+ n = name in Name n
    in

    choice ~failure_msg:"expected term" [
      let_term;
      if_then_else_term;
      add_term;
      template_term;
      text_lit_term;
      int_lit_term;
      name_term;
    ])

  let fragment : fragment t =
    let unquoted_fragment =
      let peek_end_of_unquote =
        peek_char >>= (function Some '}' -> return () | _ -> fail "")
      in

      let let_fragment =
        let+ n = token "let" *> name <* token ":="
        and+ tm = term <* peek_end_of_unquote
        in (Let (n, tm) : fragment)
      in

      let if_then_else_fragment =
        let+ tm1 = token "if" *> term
        and+ tm2 = token "then" *> string "$\"" *> fail "TODO: Template literal"
        and+ tm3 = token "else" *> string "$\"" *> fail "TODO: Template literal"
        in (IfThenElse (tm1, tm2, tm3) : fragment)
      in

      let term_fragment =
        let+ t = term in Term t
      in

      token "${" *> (* FIXME: use `%` instead? *)
      commit *>
      choice [let_fragment; if_then_else_fragment; term_fragment] <*
      string "}"
    in

    let text_fragment =
      let text_end =
        peek_char >>=
          (function
            | Some '$' | None -> return ()
            (* TODO: '"' *)
            (* TODO: '}' *)
            | Some _ -> fail "")
      in

      let+ s = many_till any_char text_end (* FIXME: Handle escapes *)
      in Text (string_of_char_list s)
    in

    choice ~failure_msg:"expected fragment" [
      unquoted_fragment;
      text_fragment;
    ]

  let template : template t =
    many_till fragment end_of_input <?> "template"

  (* Entrypoints *)

  let parse_term (source : string) : (term, string) result =
    parse_string ~consume:All term source

  let parse_template (source : string) : (template, string) result =
    parse_string ~consume:All template source

end
