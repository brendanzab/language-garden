(** Compiling a first-order functional programming language to A-normal form.

    Extends [eval_fofl].
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

module Anf = struct

  module Name : sig

    type t = private string

    val fresh : string -> t

  end = struct

    type t = string

    let fresh : string -> t =
      let next_id = ref 0 in
      fun name ->
        let id = !next_id in
        incr next_id;
        Printf.sprintf "%s$%i" name id

  end

  module Type = Core.Type

  module Expr = struct

    type t =
      | Let of Name.t * Type.t * comp * t
      | Bool_if of atom * t * t
      | Comp of comp

    and comp =
      | Prim of Prim.t * atom Iarray.t
      | Item of string * atom Iarray.t
      | Atom of atom

    and atom =
      | Item of string
      | Var of Name.t
      | Bool of bool
      | Int of int

  end

  module Item = struct

    type t =
      | Val of Type.t * Expr.t
      | Fun of (string * Type.t) Iarray.t * Type.t * Expr.t

  end

  let ( let@ ) = ( @@ )

  type item_ty_env = Type.t Core.Env.t
  type local_env = (Name.t * Type.t) Core.Env.t

  let comp_k = fun (expr, _) -> Expr.Comp expr

  let item_tys_of_program program =
    Iarray.to_seq program
    |> Seq.map (function
      | name, Core.Item.Val (ty, _) -> name, ty
      | name, Core.Item.Fun (_, ty, _) -> name, ty)
    |> Core.Env.of_seq

  let rec translate_expr (item_tys : item_ty_env) (locals : local_env) (expr : Core.Expr.t) (k : Expr.comp * Type.t  -> Expr.t) : Expr.t =
    match expr with
    | Core.Expr.Item (name, None) ->
        k (Expr.Atom (Item name), Core.Env.find name item_tys)
    | Core.Expr.Item (name, Some args) ->
        let@ args = translate_defs item_tys locals (Iarray.to_list args) in
        k (Expr.Item (name, Iarray.of_list args), Core.Env.find name item_tys)
    | Core.Expr.Var name ->
        let name', ty = Core.Env.find name locals in
        k (Expr.Atom (Var name'), ty)
    | Core.Expr.Let (name, def_ty, def, body) ->
        let name' = Name.fresh name in
        let@ def, _ = translate_expr item_tys locals def in
        let body = translate_expr item_tys (Core.Env.add name (name', def_ty) locals) body k in
        Expr.Let (name', def_ty, def, body)
    | Core.Expr.Bool bool ->
        k (Expr.Atom (Bool bool), Type.Bool)
    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        let@ expr1 = translate_def item_tys locals expr1 in
        let expr2 = translate_expr item_tys locals expr2 k in (* FIXME: join point *)
        let expr3 = translate_expr item_tys locals expr3 k in (* FIXME: join point *)
        Expr.Bool_if (expr1, expr2, expr3)
    | Core.Expr.Int int ->
        k (Expr.Atom (Int int), Type.Int)
    | Core.Expr.Prim (prim, args) ->
        let@ args = translate_defs item_tys locals (Iarray.to_list args) in
        begin match prim with
        | Prim.Int_eq -> k (Expr.Prim (prim, Iarray.of_list args), Type.Bool)
        | Prim.Int_add -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
        | Prim.Int_sub -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
        | Prim.Int_mul -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
        end

  and translate_def (item_tys : item_ty_env) (locals : local_env) (expr : Core.Expr.t) (k : Expr.atom -> Expr.t) : Expr.t =
    let@ expr, ty = translate_expr item_tys locals expr in
    match expr with
    | Expr.Atom expr -> k expr
    | expr ->
        let name' = Name.fresh "x" in
        Expr.Let (name', ty, expr, k (Expr.Var name'))

  and translate_defs (item_tys : item_ty_env) (locals : local_env) (exprs : Core.Expr.t list) (k : Expr.atom list -> Expr.t) : Expr.t =
    match exprs with
    | [] -> k []
    | expr :: exprs ->
        let@ expr = translate_def item_tys locals expr in
        let@ exprs = translate_defs item_tys locals exprs in
        k (expr :: exprs)

  let translate_item (item_tys : item_ty_env) (item : Core.Item.t) : Item.t =
    match item with
    | Core.Item.Val (ty, def) ->
        Item.Val (ty, translate_expr item_tys Core.Env.empty def comp_k)
    | Core.Item.Fun (params, ty, body) ->
        let locals =
          Iarray.to_seq params
          |> Seq.map (fun (name, ty) -> name, (Name.fresh name, ty))
          |> Core.Env.of_seq
        in
        Item.Fun (params, ty, translate_expr item_tys locals body comp_k)

  let translate_program (program : (string * Core.Item.t) Iarray.t) : (string * Item.t) Iarray.t =
    let item_tys = item_tys_of_program program in
    program |> Iarray.map @@ fun (name, item) ->
      name, translate_item item_tys item

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

    let program : _ Iarray.t = [|
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

    |] in

    let items = Iarray.to_seq program |> Env.of_seq in
    let anf_item_tys = Anf.item_tys_of_program program in
    let _anf_items = Anf.translate_program program |> Iarray.to_seq |> Env.of_seq in

    let check_eval expr expected_value =
      let value = Expr.eval items Env.empty expr in
      assert (value = expected_value);
      let _anf_expr = Anf.translate_expr anf_item_tys Env.empty expr Anf.comp_k in
      (* TODO: Ensure that the compiled version evaluates to the same value *)
      ()
    in

    test "test-fact" Expr.(fun () -> check_eval (Item ("test-fact", None)) (Value.Int 120));
    test "ackermann(0, 0)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 0; Int 0|])) (Expr.Value.Int 1));
    test "ackermann(3, 4)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 3; Int 4|])) (Value.Int 125));
    test "is-even(6)" Expr.(fun () -> check_eval (Item ("is-even", Some [|Int 6|])) (Value.Bool true));
    test "is-odd(6)" Expr.(fun () -> check_eval (Item ("is-odd", Some [|Int 6|])) (Value.Bool false));

  end

end
