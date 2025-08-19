(** Type-safe formatting strings with continuation passing.

    Based on {{: https://www.brics.dk/RS/98/12/} “Functional unparsing”} by
    Olivier Danvy.
*)

module Format : sig

  type ('a, 'b) formatter

  val lit : string -> ('a, 'a) formatter
  val eol : ('a, 'a) formatter
  val spec : ('b -> string) -> ('a, 'b -> 'a) formatter
  val int : ('a, int -> 'a) formatter
  val string : ('a, string -> 'a) formatter

  val ( ++ ) : ('b, 'c) formatter -> ('a, 'b) formatter -> ('a, 'c) formatter

  val sprintf : (string, 'a) formatter -> 'a
  val printf : (unit, 'a) formatter -> 'a [@@warning "-unused-value-declaration"]

end = struct

  type ('a, 'b) formatter = (string -> 'a) -> (string -> 'b)

  let lit x k s = k (s ^ x)
  let eol k s = lit "\n" k s
  let spec to_str k s x = lit (to_str x) k s
  let int k s = spec string_of_int k s
  let string k s = spec Fun.id k s

  let ( ++ ) f g x = f (g x)

  let sprintf p = p Fun.id ""
  let printf p = p Out_channel.(output_string stdout) ""

end

let () = begin

  assert (Format.(sprintf (lit "Hello world!" ++ eol)) = "Hello world!\n");
  assert (Format.(sprintf (string ++ lit " world!" ++ eol)) "Hello" = "Hello world!\n");
  assert (Format.(sprintf (int ++ int)) 3 4 = "34");
  assert (Format.(sprintf (spec string_of_float ++ lit " is " ++ string ++ eol)) 3.4 "x" = "3.4 is x\n");

end
