(** For embedded domain specific languages, it’s more convenient to construct
    lambda terms using higher-order abstract syntax (HOAS), but de Bruijn
    indices are usually preferred for performing intensional analysis on terms.

    This file shows how to convert from HOAS to a de Bruijn-indexed
    representation.

    Resources:

    - {{: https://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf}
      Unembedding domain-specific languages} by Atkey, Lindley, and Yallop.
*)

type index = int

type expr =
  | Let : expr * expr -> expr
  | Var : index -> expr
  | Fun_abs : expr -> expr
  | Fun_app : expr * expr -> expr
  | Int_lit : int -> expr
  | String_lit : string -> expr


(** Interface for building well-scoped expressions using HOAS *)
module Build : sig

  [@@@warning "-unused-value-declaration"]

  type t

  val run : t -> expr

  val let' : t -> (t -> t) -> t
  val fun' : (t -> t) -> t
  val app : t -> t -> t

  val int : int -> t
  val string : string -> t

  (** Notation *)

  val ( let$ ) : t -> (t -> t) -> t
  val ( $ ) : t -> t -> t

end = struct

  type t = size:int -> expr

  let run (e : t) : expr =
    e ~size:0

  let var (level : int) : t =
    fun ~size ->
      Var (size - level - 1)

  let let' (def : t) (body : t -> t) : t =
    fun ~size ->
      Let (def ~size,
        (body (var size)) ~size:(size + 1))

  let fun' (body : t -> t) : t =
    fun ~size ->
      Fun_abs ((body (var size)) ~size:(size + 1))

  let app (f : t) (a : t) : t =
    fun ~size ->
      Fun_app (f ~size, a ~size)

  let int (i : int) : t =
    fun ~size:_ -> Int_lit i

  let string (s : string) : t =
    fun ~size:_ -> String_lit s

  let ( let$ ) = let'
  let ( $ ) = app

end


let () = begin

  print_string "Running tests ...";

  begin

    let expr = Build.(
      let$ id = fun' @@ fun x -> x in
      let$ const = fun' @@ fun x -> fun' @@ fun _ -> x in
      const $ (id $ string "hello") $ int 4
    ) in

    assert (Build.run expr =
      Let (Fun_abs (Var 0),
      Let (Fun_abs (Fun_abs (Var 1)),
      Fun_app (Fun_app (Var 0, Fun_app (Var 1, String_lit "hello")), Int_lit 4))));

  end;

  print_string " ok!\n";

end
