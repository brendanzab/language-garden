(** Recursive descent parser for symbolic expressions

    - {{: https://en.wikipedia.org/wiki/S-expression} S-expression} on Wikipedia
*)


(** Lexer *)

type token =
  | Left_paren
  | Right_paren
  | Atom of string

let is_atom : char -> bool =
  function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_ascii_whitespace : char -> bool =
  function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

exception Unexpected_char of char
exception Unexpected_token of { found : token; expected : token list }
exception Unexpected_eof
exception Unconsumed_tokens of token Seq.t

let rec tokens (input : char Seq.t) : token Seq.t =
  fun () ->
    match Seq.uncons input with
    | Some (ch, input) ->
        begin match ch with
        | '(' -> Seq.Cons (Left_paren, tokens input)
        | ')' -> Seq.Cons (Right_paren, tokens input)
        | ch when is_atom ch ->
            let rec atom input acc =
              match Seq.uncons input with
              | Some (ch, input) when is_atom ch -> atom input (acc ^ String.make 1 ch)
              | Some _ | None -> Seq.Cons (Atom acc, tokens input)
            in
            atom input (String.make 1 ch)
        | ch when is_ascii_whitespace ch -> tokens input ()
        | ch -> raise (Unexpected_char ch)
        end
    | None -> Seq.Nil


(** Parser *)

type sexpr =
  | Atom of string
  | List of sexpr list

let parse_sexpr (tokens : token Seq.t) : sexpr =
  let rec parse_sexpr (tokens : token Seq.t) : sexpr * token Seq.t =
    match Seq.uncons tokens with
    | Some (Left_paren, tokens) ->
        let (sexprs, tokens) = parse_list tokens in
        (List sexprs, tokens)
    | Some (Atom s, tokens) -> Atom s, tokens
    | Some (token, _) -> raise (Unexpected_token { found = token; expected = [] })
    | None -> raise Unexpected_eof

  and parse_list (tokens : token Seq.t) : sexpr list * token Seq.t =
    match Seq.uncons tokens with
    | Some (Right_paren, tokens) -> ([], tokens)
    | Some _ | None ->
        let (sexpr, tokens) = parse_sexpr tokens in
        let (sexprs, tokens) = parse_list tokens in
        (sexpr :: sexprs, tokens)
  in

  let (sexpr, tokens) = parse_sexpr tokens in
  if Seq.is_empty tokens then sexpr else
    raise (Unconsumed_tokens tokens)


let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  assert ("()" |> String.to_seq |> tokens |> List.of_seq = [Left_paren; Right_paren]);
  assert ("foo" |> String.to_seq |> tokens |> List.of_seq = [Atom "foo"]);

  assert ("()" |> String.to_seq |> tokens |> parse_sexpr = List []);
  assert ("foo" |> String.to_seq |> tokens |> parse_sexpr = Atom "foo");
  assert ("(foo)" |> String.to_seq |> tokens |> parse_sexpr = List [Atom "foo"]);
  assert ("(foo bar)" |> String.to_seq |> tokens |> parse_sexpr = List [Atom "foo"; Atom "bar"]);
  assert ("(add 1 2 (mul 3 4))" |> String.to_seq |> tokens |> parse_sexpr = List [Atom "add"; Atom "1"; Atom "2"; List [Atom "mul"; Atom "3"; Atom "4"]]);
  assert ("(add (mul 1 2) 3 4)" |> String.to_seq |> tokens |> parse_sexpr = List [Atom "add"; List [Atom "mul"; Atom "1"; Atom "2"]; Atom "3"; Atom "4"]);

  print_string " ok!\n";

end
