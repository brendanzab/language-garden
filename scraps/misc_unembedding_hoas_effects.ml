(** Constructing well-scoped de Bruijn-indexed terms using HOAS, implemented
    with algebraic effects.

    Extends [misc_unembedding_hoas].
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

  type 'a t

  val run : (unit -> 'a t) -> 'a

  val let' : expr t -> ((unit -> expr t) -> expr t) -> expr t
  val fun' : ((unit -> expr t) -> expr t) -> expr t
  val app : expr t -> expr t -> expr t

  val int : int -> expr t
  val string : string -> expr t

  (** Notation *)

  val ( let$ ) : expr t -> ((unit -> expr t) -> expr t) -> expr t
  val ( $ ) : expr t -> expr t -> expr t

end = struct

  type 'a t = 'a

  type _ Effect.t +=
    | Size : int Effect.t

  let size () = Effect.perform Size

  let run (type a) ~(init : int) (f : unit -> a t) : a =
    let open Effect.Deep in
    try f () with
    | effect Size, k -> continue k init

  let inst (body : (unit -> expr t) -> expr t) : expr =
    let level = size () in
    run ~init:(size () + 1) @@ fun () ->
      body (fun () -> Var (size () - level - 1))

  let let' (def : expr t) (body : (unit -> expr t) -> expr t) : expr t =
    Let (def, inst body)

  let fun' (body : (unit -> expr t) -> expr t) : expr t =
    Fun_abs (inst body)

  let app (f : expr t) (a : expr t) : expr t =
    Fun_app (f, a)

  let int (i : int) : expr t =
    Int_lit i

  let string (s : string) : expr t =
    String_lit s

  let run f = run ~init:0 f

  let ( let$ ) = let'
  let ( $ ) = app

end


let () = begin

  Printexc.record_backtrace true;

  Printf.printf "Running tests in %s ..." __FILE__;

  begin

    let expr () = Build.(
      let$ id = fun' @@ fun x -> x () in
      let$ const = fun' @@ fun x -> fun' @@ fun _ -> x () in
      const () $ (id () $ string "hello") $ int 4
    ) in

    assert (Build.run expr =
      Let (Fun_abs (Var 0),
      Let (Fun_abs (Fun_abs (Var 1)),
      Fun_app (Fun_app (Var 0, Fun_app (Var 1, String_lit "hello")), Int_lit 4))));

  end;

  Printf.printf " ok!\n";

end
