(** A demonstration of compiling a first-order functional programming language
    to A-normal form.

    By “first-order” we mean that all of the functions are defined at the
    top-level, which should make it easier to generate LLVM or WASM code, as we
    don't need to worry about closures or currying (yet).

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


(** Core language *)
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
      | Int int -> Value.Int int
      | Prim (prim, args) ->
          Prim.app prim (Iarray.map (eval items locals) args)

  end

  module Program = struct

    type t = Item.t Env.t

    let item_ty (program : t) (name : string) : Type.t =
      match Env.find name program with
      | Item.Val (ty, _) -> ty
      | Item.Fun (_, ty, _) -> ty

  end

end


(** A space of names that are structurally equivalent. This is effectively a
    wrapper around strings, but with an additional layer of type safety.

    {2 Properties}

    - [compare (make n) (make n) = 0].
*)
module Label = struct

  module type S = sig

    type t

    val make : string -> t
    val compare : t -> t -> int

  end

  module Make () : S = struct

    type t = string

    let make name = name
    let compare = String.compare

  end

end


(** A space of fresh names.

    {2 Properties}

    -  [compare (fresh n) (fresh n) <> 0]
*)
module Fresh = struct

  module type S = sig

    type t

    val fresh : string -> t
    val compare : t -> t -> int

  end

  module Make () : S = struct

    type t = int * string

    let next_id = ref 0

    let fresh (name : string) : t =
      let id = !next_id in
      incr next_id;
      id, name

    let compare (i1, _) (i2, _) = Int.compare i1 i2

  end

end


(** The language in A-Normal Form, with the results of computations bound to
    intermediate bindings. This should be easier to compile to other backends,
    like LLVM-IR or Wasm. *)
module Anf = struct

  module Item_name = Label.Make ()
  module Local_name = Fresh.Make ()

  module Item_env = Map.Make (Item_name)
  module Local_env = Map.Make (Local_name)

  module Type = Core.Type

  module rec Expr : sig

    type t =
      | Let of Local_name.t * Type.t * comp * t
      | Bool_if of atom * t * t
      | Comp of comp

    and comp =
      | Prim of Prim.t * atom Iarray.t
      | Item of Item_name.t * atom Iarray.t
      | Atom of atom

    and atom =
      | Item of Item_name.t
      | Var of Local_name.t
      | Bool of bool
      | Int of int

    module Value : sig

      type t =
        | Bool of bool
        | Int of int

    end

    val eval : Item.t Item_env.t -> Value.t Local_env.t -> t -> Value.t

  end = struct

    include Expr

    module Value = struct

      type t = Prim.value =
        | Bool of bool
        | Int of int

    end

    let rec eval (items : Item.t Item_env.t) (locals : Value.t Local_env.t) (expr : t) : Value.t =
      match expr with
      | Let (name, _, def, body) ->
          let def = eval_comp items locals def in
          eval items (Local_env.add name def locals) body
      | Bool_if (expr1, expr2, expr3) ->
          begin match eval_atom items locals expr1 with
          | Value.Bool true -> eval items locals expr2
          | Value.Bool false -> eval items locals expr3
          | _ -> failwith "Expr.eval"
          end
      | Comp expr -> eval_comp items locals expr

    and eval_comp (items : Item.t Item_env.t) (locals : Value.t Local_env.t) (expr : comp) : Value.t =
      match expr with
      | Prim (prim, args) ->
          Prim.app prim (Iarray.map (eval_atom items locals) args)
      | Item (name, args) ->
          begin match Item_env.find name items with
          | Item.Fun (names, _, body) ->
              let eval_arg (name, _) arg = name, eval_atom items locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq names) (Iarray.to_seq args) in
              eval items (Local_env.add_seq args locals) body
          | _ -> failwith "Expr.eval_comp"
          end
      | Atom expr -> eval_atom items locals expr

    and eval_atom (items : Item.t Item_env.t) (locals : Value.t Local_env.t) (expr : atom) : Value.t =
      match expr with
      | Item name ->
          begin match Item_env.find name items with
          | Item.Val (_, body) -> eval items locals body
          | _ -> failwith "Expr.eval_atom"
          end
      | Var name -> Local_env.find name locals
      | Bool bool -> Value.Bool bool
      | Int int -> Value.Int int

  end

  and Item : sig

    type t =
      | Val of Type.t * Expr.t
      | Fun of (Local_name.t * Type.t) Iarray.t * Type.t * Expr.t

  end = Item

  module Program = struct

    type t = Item.t Item_env.t

  end

  type local_env = (Local_name.t * Type.t) Core.Env.t

  let translate_expr (item_tys : string -> Type.t) (locals : local_env) (expr : Core.Expr.t) =
    let ( let@ ) = ( @@ ) in

    let rec go_expr (locals : local_env) (expr : Core.Expr.t) (k : Expr.comp * Type.t -> Expr.t) : Expr.t =
      match expr with
      | Core.Expr.Item (name, None) ->
          k (Expr.Atom (Item (Item_name.make name)), item_tys name)
      | Core.Expr.Item (name, Some args) ->
          let@ args = go_defs locals (Iarray.to_list args) in
          k (Expr.Item (Item_name.make name, Iarray.of_list args), item_tys name)
      | Core.Expr.Var name ->
          let name', ty = Core.Env.find name locals in
          k (Expr.Atom (Var name'), ty)
      | Core.Expr.Let (name, def_ty, def, body) ->
          let name' = Local_name.fresh name in
          let@ def, _ = go_expr locals def in
          let body = go_expr (Core.Env.add name (name', def_ty) locals) body k in
          Expr.Let (name', def_ty, def, body)
      | Core.Expr.Bool bool ->
          k (Expr.Atom (Bool bool), Type.Bool)
      | Core.Expr.Bool_if (expr1, expr2, expr3) ->
          let@ expr1 = go_def locals expr1 in
          let expr2 = go_expr locals expr2 k in (* FIXME: join point *)
          let expr3 = go_expr locals expr3 k in (* FIXME: join point *)
          Expr.Bool_if (expr1, expr2, expr3)
      | Core.Expr.Int int ->
          k (Expr.Atom (Int int), Type.Int)
      | Core.Expr.Prim (prim, args) ->
          let@ args = go_defs locals (Iarray.to_list args) in
          begin match prim with
          | Prim.Int_eq -> k (Expr.Prim (prim, Iarray.of_list args), Type.Bool)
          | Prim.Int_add -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
          | Prim.Int_sub -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
          | Prim.Int_mul -> k (Expr.Prim (prim, Iarray.of_list args), Type.Int)
          end

    (* Compile an expression, and binding it to an intermediate definition if necessary *)
    and go_def (locals : local_env) (expr : Core.Expr.t) (k : Expr.atom -> Expr.t) : Expr.t =
      let@ expr, ty = go_expr locals expr in
      match expr with
      | Expr.Atom expr -> k expr
      | expr ->
          let name' = Local_name.fresh "x" in
          Expr.Let (name', ty, expr, k (Expr.Var name'))

    (* Compile a series of expressions to intermediate definitions *)
    and go_defs (locals : local_env) (exprs : Core.Expr.t list) (k : Expr.atom list -> Expr.t) : Expr.t =
      match exprs with
      | [] -> k []
      | expr :: exprs ->
          let@ expr = go_def locals expr in
          let@ exprs = go_defs locals exprs in
          k (expr :: exprs)
    in

    let@ expr, _ = go_expr locals expr in
    Expr.Comp expr

  let translate_item (item_tys : string -> Type.t) (item : Core.Item.t) : Item.t =
    match item with
    | Core.Item.Val (ty, def) ->
        Item.Val (ty, translate_expr item_tys Core.Env.empty def)
    | Core.Item.Fun (params, ty, body) ->
        let params' = params |> Iarray.map (fun (name, ty) -> Local_name.fresh name, ty) in
        let locals = Seq.zip (Iarray.to_seq params |> Seq.map fst) (Iarray.to_seq params') |> Core.Env.of_seq in
        Item.Fun (params', ty, translate_expr item_tys locals body)

  let translate_program (program : Core.Program.t) : Program.t =
    Core.Env.to_seq program
    |> Seq.map (fun (name, item) ->
      Item_name.make name,
      translate_item (Core.Program.item_ty program) item)
    |> Item_env.of_seq

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

    let program = Env.of_list [
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

    let anf_items = Anf.translate_program program in

    let decode_anf_value anf_value =
      match anf_value with
      | Anf.Expr.Value.Bool b -> Expr.Value.Bool b
      | Anf.Expr.Value.Int i -> Expr.Value.Int i
    in

    let check_eval expr expected_value =
      let value = Expr.eval program Env.empty expr in
      assert (value = expected_value);

      let anf_expr = Anf.translate_expr (Program.item_ty program) Env.empty expr in
      let anf_value = Anf.Expr.eval anf_items Anf.Local_env.empty anf_expr in
      assert (decode_anf_value anf_value = expected_value);
    in

    test "test-fact" Expr.(fun () -> check_eval (Item ("test-fact", None)) (Value.Int 120));
    test "ackermann(0, 0)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 0; Int 0|])) (Expr.Value.Int 1));
    test "ackermann(3, 4)" Expr.(fun () -> check_eval (Item ("ackermann", Some [|Int 3; Int 4|])) (Value.Int 125));
    test "is-even(6)" Expr.(fun () -> check_eval (Item ("is-even", Some [|Int 6|])) (Value.Bool true));
    test "is-odd(6)" Expr.(fun () -> check_eval (Item ("is-odd", Some [|Int 6|])) (Value.Bool false));

  end

end
