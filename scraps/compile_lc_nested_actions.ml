(** An example of translating “!-notation”/“nested actions” to monadic
    bind operations.

    Monadic code (in languages like Haskell and OCaml) usually requires that the
    result of each effectful computation be bound to an intermediate result.
    This results in verbose, unnatural code. Idris and Lean on the other hand
    allow effectful code to be expressed in a more direct style with
    “!-notation” and “nested actions” respectively.

    For example:

    {@text[
      let x <- readInt;
      let y <- readInt;
      printInt (x + y)
    ]}

    Could be expressed more directly as:

    {@text[
      printInt (!readInt + !readInt)
    ]}

    Where the nested actions will be executed sequentially in a depth-first,
    left-to-right order.

    {2 Todo}

    It might be nice to add some additional features like:

    - Idiom brackets for expressing parallel computations
    - Monad comprehensions

    {2 Resources}

    - {{: https://www.type-driven.org.uk/edwinb/papers/dep-eff.pdf}
      Resource-dependent Algebraic Effects § 2.2 !-notation}
    - {{: https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation}
      The Idris Tutorial § !-notation}
    - {{: https://lean-lang.org/doc/reference/latest/Functors___-Monads-and--do--Notation/Syntax}
      The Lean Language Reference § 18.3.2.1. Sequential Computations}
*)

(** Fresh variable names *)
module Id : sig

  type t

  val fresh : unit -> t
  val compare : t -> t -> int

end = struct

  type t = int

  let next_id = ref 0

  let fresh () : t =
    let id = !next_id in
    incr next_id;
    id

  let compare = Int.compare

end

(** The source language *)
module L1 = struct

  module Expr = struct

    type t =
      | Var of Id.t                 (* x *)
      | Let of Id.t * t * t         (* let x := e; e *)
      | Eff_bind of Id.t * t * t    (* let x <- e; e *)   [@warning "-unused-constructor"]
      | Eff_pure of t               (* pure e *)          [@warning "-unused-constructor"]
      | Eff_action of t             (* !e *)
      | Fun_lit of Id.t * t         (* \x. e *)           [@warning "-unused-constructor"]
      | Fun_app of t * t            (* e e *)

  end

end

(** The target language *)
module L2 = struct

  module Expr = struct

    type t =
      | Var of Id.t                 (* x *)
      | Let of Id.t * t * t         (* let x := e; e *)
      | Eff_bind of Id.t * t * t    (* let x <- e; e *)
      | Eff_pure of t               (* pure e *)
      | Fun_lit of Id.t * t         (* \x. e *)
      | Fun_app of t * t            (* e e *)

  end

end

(** Monadic code looks quite similar to A-normal form! This suggests that we can
    use similar approach to {{: https://matt.might.net/articles/a-normalization/}
    ANF-translation} to turn nested actions in the source language into bind
    operations in the target language.

    Like in ANF-translation, our translation function takes a continuation that
    represents parts of the expression that surround the expression that we are
    currently translating. This allows us to wrap the translated expression in a
    new monadic binding whenever we hit a nested action.

    I assume a similar approach could be used to elaborate direct-style code to
    CBPV in a type-directed way (as described by Andras Kovacs in this comment:
    https://www.reddit.com/r/ProgrammingLanguages/comments/1bam7bd/im_betting_on_callbypushvalue/ku4yel3/).
*)

let ( let@ ) = ( @@ )

let rec translate (expr : L1.Expr.t) (k : L2.Expr.t -> L2.Expr.t) : L2.Expr.t =
  match expr with
  | L1.Expr.Var x ->
      k (L2.Expr.Var x)
  | L1.Expr.Let (x, def, body) ->
      let@ def = translate def in
      L2.Expr.Let (x, def, translate body k)
  | L1.Expr.Eff_bind (x, def, body) ->
      let@ def = translate def in
      L2.Expr.Eff_bind (x, def, translate body k)
  | L1.Expr.Eff_pure expr ->
      let@ expr = translate expr in
      k (L2.Expr.Eff_pure expr)

  (* Lift the expression as high as possible in the current scope, binding
     it to a fresh name. *)
  | L1.Expr.Eff_action expr ->
      let x = Id.fresh () in
      let@ expr = translate expr in
      L2.Expr.Eff_bind (x, expr, k (Var x))

  | L1.Expr.Fun_lit (x, body) ->
      k (L2.Expr.Fun_lit (x, translate body Fun.id))
  | L1.Expr.Fun_app (head, arg) ->
      let@ head = translate head in
      let@ arg = translate arg in
      k (L2.Expr.Fun_app (head, arg))

let translate (e : L1.Expr.t) : L2.Expr.t =
  translate e Fun.id


(* Tests *)

let () = begin

  Printexc.record_backtrace true;

  let run_tests (type a) (prog : (string -> (unit -> unit) -> unit) -> unit) : a =
    let success_count = ref 0 in
    let error_count = ref 0 in

    let run_test (name : string) (prog : unit -> unit) : unit =
      Printf.printf "test %s ... " name;

      match prog () with
      | () ->
          Printf.printf "ok\n";
          incr success_count
      | exception e ->
          Printf.printf "error:\n\n";
          Printf.printf "  %s\n\n" (Printexc.to_string e);
          String.split_on_char '\n' (Printexc.get_backtrace()) |> List.iter begin fun line ->
            Printf.printf "  %s\n" line;
          end;
          incr error_count
    in

    Printf.printf "Running tests in %s:\n\n" __FILE__;
    prog run_test;
    Printf.printf "\n";

    if !error_count <= 0 then begin
      Printf.printf "Ran %i successful tests\n\n" !success_count;
      exit 0
    end else begin
      Printf.printf "Failed %i out of %i tests\n\n" !error_count (!success_count + !error_count);
      exit 1
    end
  in

  begin run_tests @@ fun test ->

    begin test "simple" @@ fun () ->

      let print_int = Id.fresh () in
      let read_int = Id.fresh () in
      let add_int = Id.fresh () in

      (*
        printInt (!readInt + !readInt)
      *)
      let src = L1.Expr.(
        let ( ! ) e = Eff_action e in
        let ( $ ) f x = Fun_app (f, x) in
        Var print_int $ (Var add_int $ !(Var read_int) $ !(Var read_int))
      ) in

      (*
        let x <- readInt;
        let y <- readInt;
        printInt (x + y)
      *)
      match translate src with
      | Eff_bind (fresh_x, Var read_int',
          Eff_bind (fresh_y, Var read_int'',
            Fun_app (Var print_int', Fun_app (Fun_app (Var add_int', Var fresh_x'), Var fresh_y')))) ->
          assert (Id.compare read_int read_int' = 0);
          assert (Id.compare read_int read_int'' = 0);
          assert (Id.compare print_int print_int' = 0);
          assert (Id.compare add_int add_int' = 0);
          assert (Id.compare fresh_x fresh_x' = 0);
          assert (Id.compare fresh_y fresh_y' = 0);
      | _ ->
          assert false

    end;

    (* Example from https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation *)
    begin test "idris example" @@ fun () ->
      let y = Id.fresh () in
      let _42 = Id.fresh () in
      let f = Id.fresh () in
      let g = Id.fresh () in
      let print = Id.fresh () in
      let x = Id.fresh () in

      (*
        let y := 42;
        f !(g !(print y) !x)
      *)
      let src = L1.Expr.(
        let ( ! ) e = Eff_action e in
        let ( $ ) f x = Fun_app (f, x) in
        Let (y, Var _42, Var f $ !(Var g $ !(Var print $ Var y) $ !(Var x)))
      ) in

      (*
        let y = 42;
        let $y <- print y;
        let $x <- x;
        let $g <- g $y $x;
        f $g
      *)
      match translate src with
      | Let (y', Var _42',
          Eff_bind (fresh_y, Fun_app (Var print', Var y''),
            Eff_bind (fresh_x, Var x',
              Eff_bind (fresh_g, Fun_app (Fun_app (Var g', Var fresh_y'), Var fresh_x'),
                Fun_app (Var f', Var fresh_g'))))) ->
          assert (Id.compare y y' = 0);
          assert (Id.compare _42 _42' = 0);
          assert (Id.compare print print' = 0);
          assert (Id.compare y y'' = 0);
          assert (Id.compare x x' = 0);
          assert (Id.compare g g' = 0);
          assert (Id.compare fresh_y fresh_y' = 0);
          assert (Id.compare fresh_x fresh_x' = 0);
          assert (Id.compare fresh_g fresh_g' = 0);
          assert (Id.compare f f' = 0);
      | _ ->
          assert false

    end;

  end

end
