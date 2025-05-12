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

exception Unexpected_char of { found : char }

let[@tail_mod_cons] rec tokens (input : char Seq.t) : token Seq.t =
  match Seq.uncons input with
  | Some (ch, input) ->
      begin match ch with
      | '(' -> Seq.cons Left_paren (tokens input)
      | ')' -> Seq.cons Right_paren (tokens input)
      | ch when is_atom ch ->
          let buf = Buffer.create 1 in
          Buffer.add_char buf ch;
          let rec atom input =
            match Seq.uncons input with
            | Some (ch, input) when is_atom ch -> Buffer.add_char buf ch; atom input
            | Some _ | None -> (String.of_bytes (Buffer.to_bytes buf), input)
          in
          let (s, input) = atom input in
          Seq.cons (Atom s) (tokens input)
      | ch when is_ascii_whitespace ch -> tokens input
      | ch -> raise (Unexpected_char { found = ch })
      end
  | None -> Seq.empty


(** Parser *)

exception Unexpected_token of { found : token }
exception Unexpected_eof
exception Unconsumed_tokens of { remaining : token Seq.t }

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
    | Some (token, _) -> raise (Unexpected_token { found = token })
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
    raise (Unconsumed_tokens { remaining = tokens })


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
