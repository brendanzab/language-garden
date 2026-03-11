(** Evaluation for a simple, first-order functional language with top-level
    items and mutual recursion.
*)

(** Primitive values and operations *)
module Prim = struct

  module Value = struct

    type t =
      | Bool of bool
      | I32 of int32

  end

  module Op = struct

    type t =
      | I32_eq
      | I32_add
      | I32_sub
      | I32_mul

    let app (op : t) (args : Value.t Iarray.t) : Value.t =
      match op, args with
      | I32_eq, Value.[|I32 int1; I32 int2|] -> Value.Bool (Int32.equal int1 int2)
      | I32_add, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.add int1 int2)
      | I32_sub, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.sub int1 int2)
      | I32_mul, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.mul int1 int2)
      | _, _ -> failwith "Prim.Op.app"

  end

end

module Core = struct

  module Env = Map.Make (String)

  module Type = struct

    type t =
      | Bool
      | I32

  end

  module rec Item : sig

    type t =
      | Val of Type.t * Expr.t
      | Fun of (string * Type.t) Iarray.t * Type.t * Expr.t

  end = Item

  and Expr : sig

    type t =
      | Item of string * t Iarray.t option
      | Var of string
      | Let of string * Type.t * t * t
      | Bool of bool
      | Bool_if of t * t * t
      | I32 of int32
      | Prim of Prim.Op.t * t Iarray.t

    module Value : sig

      type t =
        | Bool of bool
        | I32 of int32

    end

    val eval : Item.t Env.t -> Value.t Env.t -> t -> Value.t

  end = struct

    include Expr

    module Value = Prim.Value

    let rec eval (items : Item.t Env.t) (locals : Value.t Env.t) (expr : t) : Value.t =
      match expr with
      | Item (name, args) ->
          begin match Env.find name items, args with
          | Item.Val (_, body), None ->
              eval items locals body
          | Item.Fun (names, _, body), Some args ->
              let eval_arg (name, _) arg = name, eval items locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq names) (Iarray.to_seq args) in
              eval items (Env.add_seq args locals) body
          | _, _ -> failwith "Expr.eval"
          end
      | Var name -> Env.find name locals
      | Let (name, _, def, body) ->
          let def = eval items locals def in
          eval items (Env.add name def locals) body
      | Bool bool -> Value.Bool bool
      | Bool_if (expr1, expr2, expr3) ->
          begin match eval items locals expr1 with
          | Value.Bool true -> eval items locals expr2
          | Value.Bool false -> eval items locals expr3
          | _ -> failwith "Expr.eval"
          end
      | I32 int -> Value.I32 int
      | Prim (op, args) ->
          Prim.Op.app op (Iarray.map (eval items locals) args)

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

    let open Core in
    let open Prim.Op in

    let items = Env.of_list [
      "test-fact", Item.Val (
        Type.I32,
        Expr.Item ("fact", Some [|Expr.I32 5l|])
      );

      "fact", Item.Fun (
        [|"n", Type.I32|],
        Type.I32,
        Expr.Bool_if (
          Expr.Prim (I32_eq, [|Expr.Var "n"; Expr.I32 0l|]),
          Expr.I32 1l,
          Expr.Prim (I32_mul, [|
            Expr.Var "n";
            Expr.Item ("fact", Some [|Expr.Prim (I32_sub, [|Expr.Var "n"; Expr.I32 1l|])|])
          |])
        );
      );

      "ackermann", Item.Fun (
        [|"m", Type.I32; "n", Type.I32|],
        Type.I32,
        Expr.Bool_if (
          Expr.Prim (I32_eq, [|Expr.Var "m"; Expr.I32 0l|]),
          Expr.Prim (I32_add, [|Expr.Var "n"; Expr.I32 1l|]),
          Expr.Bool_if (
            Expr.Prim (I32_eq, [|Expr.Var "n"; Expr.I32 0l|]),
            Expr.Item ("ackermann", Some [|Expr.Prim (I32_sub, [|Expr.Var "m"; Expr.I32 1l|]); Expr.I32 1l|]),
            Expr.Item ("ackermann", Some [|
              Expr.Prim (I32_sub, [|Expr.Var "m"; Expr.I32 1l|]);
              Expr.Item ("ackermann", Some [|Expr.Var "m"; Expr.Prim (I32_sub, [|Expr.Var "n"; Expr.I32 1l|])|]);
            |])
          )
        )
      );

      "is-even", Item.Fun (
        [|"n", Type.I32|],
        Type.Bool,
        Expr.Bool_if (
          Expr.Prim (I32_eq, [|Expr.Var "n"; Expr.I32 0l|]),
          Expr.Bool true,
          Expr.Item ("is-odd", Some [|Expr.Prim (I32_sub, [|Expr.Var "n"; Expr.I32 1l|])|])
        )
      );

      "is-odd", Item.Fun (
        [|"n", Type.I32|],
        Type.Bool,
        Expr.Bool_if (
          Expr.Prim (I32_eq, [|Expr.Var "n"; Expr.I32 0l|]),
          Expr.Bool false,
          Expr.Item ("is-even", Some [|Expr.Prim (I32_sub, [|Expr.Var "n"; Expr.I32 1l|])|])
        )
      );

    ] in

    let check_eval expr expected =
      assert (Expr.eval items Env.empty expr = expected)
    in

    test "test-fact" Expr.(fun () -> check_eval (Item ("test-fact", None)) (Value.I32 120l));
    test "ackermann(0, 0)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|I32 0l; I32 0l|])) (Expr.Value.I32 1l));
    test "ackermann(3, 4)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|I32 3l; I32 4l|])) (Value.I32 125l));
    test "is-even(6)" Expr.(fun () -> check_eval (Item ("is-even", Some [|I32 6l|])) (Value.Bool true));
    test "is-odd(6)" Expr.(fun () -> check_eval (Item ("is-odd", Some [|I32 6l|])) (Value.Bool false));

  end

end
