(** Evaluation for a simple, first-order functional programming language with
    top-level items and mutual recursion.

    This language could serve as a basis for exploring compilation, as it avoids
    the complexities around closures and uncurrying.
*)

(* TODO:

  - parser
  - elaborator
  - data structures (structs/unions)
  - compile to A-Normal Form
  - compile to LLVM-IR (see: https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io)
*)

module rec Item : sig

  type t =
    | Val of Expr.t
    | Fun of string list * Expr.t

end = Item

and Prim : sig

  type t =
    | Int_eq
    | Int_add
    | Int_sub
    | Int_mul

end = Prim

and Expr : sig

  type t =
    | Item of string * t list
    | Var of string
    | Let of string * t * t
    | Bool of bool
    | Bool_if of t * t * t
    | Int of int
    | Prim of Prim.t * t list

  module Value : sig

    type t =
      | Bool of bool
      | Int of int

  end

  val eval : (string * Item.t) list -> (string * Value.t) list -> t -> Value.t

end = struct

  include Expr

  let rec eval (items : (string * Item.t) list) (env : (string * Value.t) list) (expr : t) : Value.t =
    match expr with
    | Item (name, args) ->
        begin match List.assoc name items with
        | Item.Val body -> assert (List.is_empty args); eval items env body
        | Item.Fun (names, body) ->
            let args = List.map2 (fun name arg -> name, eval items env arg) names args in
            eval items (List.append args env) body
        end
    | Var name -> List.assoc name env
    | Let (name, def, body) ->
        let def = eval items env def in
        eval items ((name, def) :: env) body
    | Bool bool -> Value.Bool bool
    | Bool_if (expr1, expr2, expr3) ->
        begin match eval items env expr1 with
        | Value.Bool true -> eval items env expr2
        | Value.Bool false -> eval items env expr3
        | _ -> failwith "eval"
        end
    | Int int -> Value.Int int
    | Prim (prim, args) ->
        begin match prim, List.map (eval items env) args with
        | Prim.Int_eq, [Value.Int int1; Value.Int int2] -> Value.Bool (Int.equal int1 int2)
        | Prim.Int_add, [Value.Int int1; Value.Int int2] -> Value.Int (Int.add int1 int2)
        | Prim.Int_sub, [Value.Int int1; Value.Int int2] -> Value.Int (Int.sub int1 int2)
        | Prim.Int_mul, [Value.Int int1; Value.Int int2] -> Value.Int (Int.mul int1 int2)
        | _, _ -> failwith "eval"
        end

end

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

    let items = [
      "test-fact", Item.Val (
        Expr.Item ("fact", [Expr.Int 5])
      );

      "fact", Item.Fun (
        ["n"],
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [Expr.Var "n"; Expr.Int 0]),
          Expr.Int 1,
          Expr.Prim (Prim.Int_mul, [
            Expr.Var "n";
            Expr.Item ("fact", [Expr.Prim (Prim.Int_sub, [Expr.Var "n"; Expr.Int 1])])
          ])
        );
      );

      "ackermann", Item.Fun (
        ["m"; "n"],
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [Expr.Var "m"; Expr.Int 0]),
          Expr.Prim (Prim.Int_add, [Expr.Var "n"; Expr.Int 1]),
          Expr.Bool_if (
            Expr.Prim (Prim.Int_eq, [Expr.Var "n"; Expr.Int 0]),
            Expr.Item ("ackermann", [Expr.Prim (Prim.Int_sub, [Expr.Var "m"; Expr.Int 1]); Expr.Int 1]),
            Expr.Item ("ackermann", [
              Expr.Prim (Prim.Int_sub, [Expr.Var "m"; Expr.Int 1]);
              Expr.Item ("ackermann", [Expr.Var "m"; Expr.Prim (Prim.Int_sub, [Expr.Var "n"; Expr.Int 1])]);
            ])
          )
        )
      );

      "is-even", Item.Fun (
        ["n"],
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [Expr.Var "n"; Expr.Int 0]),
          Expr.Bool true,
          Expr.Item ("is-odd", [Expr.Prim (Prim.Int_sub, [Expr.Var "n"; Expr.Int 1])])
        )
      );

      "is-odd", Item.Fun (
        ["n"],
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [Expr.Var "n"; Expr.Int 0]),
          Expr.Bool false,
          Expr.Item ("is-even", [Expr.Prim (Prim.Int_sub, [Expr.Var "n"; Expr.Int 1])])
        )
      );

    ] in

    test "test-fact" begin fun () ->
      assert (Expr.eval items [] (Expr.Item ("test-fact", [])) = Expr.Value.Int 120);
    end;

    test "ackermann(0, 0)" begin fun () ->
      assert (Expr.eval items [] (Expr.Item ("ackermann", [Expr.Int 0; Expr.Int 0])) = Expr.Value.Int 1);
    end;

    test "ackermann(3, 4)" begin fun () ->
      assert (Expr.eval items [] (Expr.Item ("ackermann", [Expr.Int 3; Expr.Int 4])) = Expr.Value.Int 125);
    end;

    test "is-even(6)" begin fun () ->
      assert (Expr.eval items [] (Expr.Item ("is-even", [Expr.Int 6])) = Expr.Value.Bool true);
    end;

    test "is-odd(6)" begin fun () ->
      assert (Expr.eval items [] (Expr.Item ("is-odd", [Expr.Int 6])) = Expr.Value.Bool false);
    end;

  end

end
