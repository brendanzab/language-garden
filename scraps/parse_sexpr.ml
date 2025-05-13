(** Recursive descent parser for symbolic expressions

    - {{: https://en.wikipedia.org/wiki/S-expression} S-expression} on Wikipedia
*)


(** Lexer *)

type token =
  | Ident of string
  | Int of int
  | Left_paren
  | Right_paren

let is_digit (ch : char) : bool =
  match ch with
  | '0' .. '9' -> true
  | _ -> false

let is_ident_start (ch : char) : bool =
  match ch with
  (* https://www.ietf.org/archive/id/draft-rivest-sexp-01.html#name-token-representation *)
  | 'a' .. 'z' | 'A' .. 'Z' | '-' | '.' | '/' | ':' | '*' | '+' | '=' -> true
  | _ -> false

let is_ident_continue (ch : char) : bool =
  is_ident_start ch || is_digit ch

let is_ascii_whitespace (ch : char) : bool =
  match ch with
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

exception Unexpected_char of { found : char }

let[@tail_mod_cons] rec tokens (input : char Seq.t) : token Seq.t =
  let take_while f init_ch input =
    let buf = Buffer.create 1 in
    Buffer.add_char buf init_ch;
    let rec go input =
      match Seq.uncons input with
      | Some (ch, input) when f ch -> Buffer.add_char buf ch; go input
      | Some _ | None -> (String.of_bytes (Buffer.to_bytes buf), input)
    in
    go input
  in
  match Seq.uncons input with
  | Some (ch, input) ->
      begin match ch with
      | '(' -> Seq.cons Left_paren (tokens input)
      | ')' -> Seq.cons Right_paren (tokens input)
      | ch when is_digit ch ->
          let (s, input) = take_while is_digit ch input in
          Seq.cons (Int (int_of_string s)) (tokens input)
      | ch when is_ident_start ch ->
          let (s, input) = take_while is_ident_continue ch input in
          Seq.cons (Ident s) (tokens input)
      | ch when is_ascii_whitespace ch -> tokens input
      | ch -> raise (Unexpected_char { found = ch })
      end
  | None -> Seq.empty


(** Parser *)

exception Unexpected_token of { found : token }
exception Unexpected_eof
exception Unconsumed_tokens of { remaining : token Seq.t }

type sexpr =
  | Ident of string
  | Int of int
  | List of sexpr list

let parse_sexpr (tokens : token Seq.t) : sexpr =
  let rec parse_sexpr (tokens : token Seq.t) : sexpr * token Seq.t =
    match Seq.uncons tokens with
    | Some (Left_paren, tokens) -> parse_list tokens []
    | Some (Ident s, tokens) -> (Ident s, tokens)
    | Some (Int s, tokens) -> (Int s, tokens)
    | Some (token, _) -> raise (Unexpected_token { found = token })
    | None -> raise Unexpected_eof

  and parse_list (tokens : token Seq.t) (acc : sexpr list) : sexpr * token Seq.t =
    match Seq.uncons tokens with
    | Some (Right_paren, tokens) -> (List (List.rev acc), tokens)
    | Some _ | None ->
        let (sexpr, tokens) = parse_sexpr tokens in
        parse_list tokens (sexpr :: acc)
  in

  let (sexpr, tokens) = parse_sexpr tokens in
  if Seq.is_empty tokens then sexpr else
    raise (Unconsumed_tokens { remaining = tokens })


let () = begin

  let tokenise input = String.to_seq input |> tokens in
  let parse input = tokenise input |> parse_sexpr in

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  assert (tokenise "()" |> List.of_seq = [Left_paren; Right_paren]);
  assert (tokenise "foo" |> List.of_seq = [Ident "foo"]);

  assert (parse "()" = List []);
  assert (parse "foo" = Ident "foo");
  assert (parse "(foo)" = List [Ident "foo"]);
  assert (parse "(foo bar)" = List [Ident "foo"; Ident "bar"]);
  assert (parse "(+ 1 2 (* 3 4))" = List [Ident "+"; Int 1; Int 2; List [Ident "*"; Int 3; Int 4]]);
  assert (parse "(+ (* 1 2) 3 4)" = List [Ident "+"; List [Ident "*"; Int 1; Int 2]; Int 3; Int 4]);

  print_string " ok!\n";

end
