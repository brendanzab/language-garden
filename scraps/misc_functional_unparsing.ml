(** Type-safe formatting strings with continuation passing.

    Based on {{: https://www.brics.dk/RS/98/12/} “Functional unparsing”} by
    Olivier Danvy. This approach is used as the basis of OCaml’s format strings,
    as implemented in the {!CamlinternalFormatBasics} module.

    Resources:

    - Olivier Danvy, {{: https://www.brics.dk/RS/98/12/} Functional unparsing}
    - Oleg Kiselyov, {{: https://okmij.org/ftp/typed-formatting/FPrintScan.html}
      Type-safe functional formatted IO}
    - {{: https://ocaml.org/manual/5.3/api/Stdlib.html#1_Operationsonformatstrings}
      OCaml Stdlib: Operations on format strings}
*)

module Format : sig

  type ('a, 'b) t
  (** The type of format strings, parameterised by:

      - ['a]: The output returned from a fully applied formatting function. For
              example {!printf} will return {!unit} and {!sprintf}
              will return {!string}.
      - ['b]: A function type that accumulates the type of each format specifier
              followed by the output type. For example:
              ['spec_0 -> ... -> 'spec_n -> 'a].
  *)

  val ( ++ ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Compose two format strings. *)

  val lit : string -> ('a, 'a) t
  val eol : ('a, 'a) t

  val spec : ('b -> string) -> ('a, 'b -> 'a) t
  val int : ('a, int -> 'a) t
  val string : ('a, string -> 'a) t

  (** Conversion of format strings into formatting functions *)

  val sprintf : (string, 'a) t -> 'a
  val printf : (unit, 'a) t -> 'a [@@warning "-unused-value-declaration"]
  val kprintf : (string -> 'a) -> ('a, 'b) t -> 'b [@@warning "-unused-value-declaration"]

end = struct

  type ('a, 'b) t = (string -> 'a) -> (string -> 'b)

  let lit x k s = k (s ^ x)
  let eol k s = lit "\n" k s

  let spec to_str k s x = lit (to_str x) k s
  let int k s = spec string_of_int k s
  let string k s = spec Fun.id k s

  let ( ++ ) = Fun.compose

  let kprintf k p = p k ""
  let sprintf p = kprintf Fun.id p
  let printf p = kprintf Out_channel.(output_string stdout) p

end

let () = begin

  assert (Format.(sprintf (lit "Hello world!" ++ eol)) = "Hello world!\n");
  assert (Format.(sprintf (string ++ lit " world!" ++ eol)) "Hello" = "Hello world!\n");
  assert (Format.(sprintf (int ++ int)) 3 4 = "34");
  assert (Format.(sprintf (spec string_of_float ++ lit " is " ++ string ++ eol)) 3.4 "x" = "3.4 is x\n");

end
