(** Evaluation for a simple, first-order functional programming language with
    top-level items and mutual recursion.

    This language could serve as a basis for exploring compilation, as it avoids
    the complexities around closures and uncurrying.
*)

module Prim = struct

  type t =
    | Int_eq
    | Int_add
    | Int_sub
    | Int_mul

  type value =
    | Bool of bool
    | Int of int

  let app (op : t) (args : value Iarray.t) =
    match op, args with
    | Int_eq, [|Int int1; Int int2|] -> Bool (Int.equal int1 int2)
    | Int_add, [|Int int1; Int int2|] -> Int (Int.add int1 int2)
    | Int_sub, [|Int int1; Int int2|] -> Int (Int.sub int1 int2)
    | Int_mul, [|Int int1; Int int2|] -> Int (Int.mul int1 int2)
    | _, _ -> failwith "Prim.app"

end

module Core = struct

  module Env = Map.Make (String)

  module Type = struct

    type t =
      | Bool
      | Int

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
      | Int of int
      | Prim of Prim.t * t Iarray.t

    module Value : sig

      type t =
        | Bool of bool
        | Int of int

    end

    val eval : Item.t Env.t -> Value.t Env.t -> t -> Value.t

  end = struct

    include Expr

    module Value = struct

      type t = Prim.value =
        | Bool of bool
        | Int of int

    end

    let rec eval (items : Item.t Env.t) (locals : Value.t Env.t) (expr : t) : Value.t =
      match expr with
      | Item (name, args) ->
          begin match Env.find name items, args with
          | Item.Val (_, body), None ->
              eval items locals body
          | Item.Fun (names, _, body), Some args ->
              let args =
                Seq.map2 (fun (name, _) arg -> name, eval items locals arg)
                  (Iarray.to_seq names)
                  (Iarray.to_seq args)
              in
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
      | Int int -> Value.Int int
      | Prim (prim, args) ->
          Prim.app prim (Iarray.map (eval items locals) args)

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

    let items = Env.of_list [
      "test-fact", Item.Val (
        Type.Int,
        Expr.Item ("fact", Some [|Expr.Int 5|])
      );

      "fact", Item.Fun (
        [|"n", Type.Int|],
        Type.Int,
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [|Expr.Var "n"; Expr.Int 0|]),
          Expr.Int 1,
          Expr.Prim (Prim.Int_mul, [|
            Expr.Var "n";
            Expr.Item ("fact", Some [|Expr.Prim (Prim.Int_sub, [|Expr.Var "n"; Expr.Int 1|])|])
          |])
        );
      );

      "ackermann", Item.Fun (
        [|"m", Type.Int; "n", Type.Int|],
        Type.Int,
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [|Expr.Var "m"; Expr.Int 0|]),
          Expr.Prim (Prim.Int_add, [|Expr.Var "n"; Expr.Int 1|]),
          Expr.Bool_if (
            Expr.Prim (Prim.Int_eq, [|Expr.Var "n"; Expr.Int 0|]),
            Expr.Item ("ackermann", Some [|Expr.Prim (Prim.Int_sub, [|Expr.Var "m"; Expr.Int 1|]); Expr.Int 1|]),
            Expr.Item ("ackermann", Some [|
              Expr.Prim (Prim.Int_sub, [|Expr.Var "m"; Expr.Int 1|]);
              Expr.Item ("ackermann", Some [|Expr.Var "m"; Expr.Prim (Prim.Int_sub, [|Expr.Var "n"; Expr.Int 1|])|]);
            |])
          )
        )
      );

      "is-even", Item.Fun (
        [|"n", Type.Int|],
        Type.Bool,
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [|Expr.Var "n"; Expr.Int 0|]),
          Expr.Bool true,
          Expr.Item ("is-odd", Some [|Expr.Prim (Prim.Int_sub, [|Expr.Var "n"; Expr.Int 1|])|])
        )
      );

      "is-odd", Item.Fun (
        [|"n", Type.Int|],
        Type.Bool,
        Expr.Bool_if (
          Expr.Prim (Prim.Int_eq, [|Expr.Var "n"; Expr.Int 0|]),
          Expr.Bool false,
          Expr.Item ("is-even", Some [|Expr.Prim (Prim.Int_sub, [|Expr.Var "n"; Expr.Int 1|])|])
        )
      );

    ] in

    let check_eval expr expected =
      assert (Expr.eval items Env.empty expr = expected)
    in

    test "test-fact" Expr.(fun () -> check_eval (Item ("test-fact", None)) (Value.Int 120));
    test "ackermann(0, 0)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 0; Int 0|])) (Expr.Value.Int 1));
    test "ackermann(3, 4)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 3; Int 4|])) (Value.Int 125));
    test "is-even(6)" Expr.(fun () -> check_eval (Item ("is-even", Some [|Int 6|])) (Value.Bool true));
    test "is-odd(6)" Expr.(fun () -> check_eval (Item ("is-odd", Some [|Int 6|])) (Value.Bool false));

  end

end
