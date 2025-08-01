(** For embedded domain specific languages, it’s more convenient to construct
    lambda terms using higher-order abstract syntax (HOAS), but de Bruijn
    indices are usually preferred for performing intensional analysis on terms.

    This file shows how to convert from typed HOAS to a well-typed, de Bruijn-
    indexed representation.

    Resources:

    - {{: https://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf}
      Unembedding domain-specific languages} by Atkey, Lindley, and Yallop.
*)

type ('ctx, 'a) index =
  | Stop : ('a * 'ctx, 'a) index
  | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

type ('ctx, 'a) expr =
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Var : ('ctx, 'a) index -> ('ctx, 'a) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
  | Int_lit : int -> ('ctx, int) expr
  | String_lit : string -> ('ctx, string) expr


(** Interface for building well-typed expressions using HOAS *)
module Build : sig

  [@@@warning "-unused-value-declaration"]

  type 'a t

  val run : 'a t -> (unit, 'a) expr

  val let' : 'a t -> ('a t -> 'b t) -> 'b t
  val fun' : ('a t -> 'b t) -> ('a -> 'b) t
  val app : ('a -> 'b) t -> 'a t -> 'b t

  val int : int -> int t
  val string : string -> string t

  (** Notation *)

  val ( let$ ) : 'a t -> ('a t -> 'b t) -> 'b t
  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

end = struct

  type 'a t = {
    run : 'ctx. size:int -> ('ctx, 'a) expr;
  }

  let run (e : 'a t) : (unit, 'a) expr =
    e.run ~size:0

  let rec index (type ctx a) (i : int) : (ctx, a) index =
    Obj.magic (if i > 0 then Pop (index (i - 1)) else Stop)

  let var (type a) (level : int) : a t =
    { run = fun ~size ->
        Var (index (size - level - 1));
    }

  let let' (type a b) (def : a t) (body : a t -> b t) : b t =
    { run = fun ~size ->
        Let (def.run ~size,
          (body (var size)).run ~size:(size + 1));
    }

  let fun' (type a b) (body : a t -> b t) : (a -> b) t =
    { run = fun ~size ->
        Fun_abs ((body (var size)).run ~size:(size + 1));
    }

  let app (type a b) (f : (a -> b) t) (a : a t) : b t =
    { run = fun ~size ->
        Fun_app (f.run ~size, a.run ~size);
    }

  let int (i : int) : int t =
    { run = fun ~size:_ -> Int_lit i }

  let string (s : string) : string t =
    { run = fun ~size:_ -> String_lit s }

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
      Let (Fun_abs (Var Stop),
      Let (Fun_abs (Fun_abs (Var (Pop Stop))),
      Fun_app (Fun_app (Var Stop, Fun_app (Var (Pop Stop), String_lit "hello")), Int_lit 4))));

  end;

  print_string " ok!\n";

end
