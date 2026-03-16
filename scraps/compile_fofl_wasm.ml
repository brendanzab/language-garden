(** A demonstration of compiling a first-order functional programming language
    to Web Assembly.

    Extends [eval_fofl].

    We first hoist let-bindings and conditionals to the top of expressions. The
    resulting program is then emitted to the WebAssembly Text Format:

    {@text[
            Core.Program.t
                  │
                  │    Hoist_lets.translate_program
                  │
                  ▼
        Let_hoisted.Program.t
                  │
                  │    Emit_wat.pp_program
                  │
                  ▼
      Format.formatter -> unit
    ]}

    A tool like {{: https://github.com/WebAssembly/binaryen} wasm-opt} could
    be used to perform optimisations to the resulting WASM.
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


module Item_name : sig

  type t

  val make : string -> t
  val compare : t -> t -> int
  val to_string : t -> string

end = struct

  type t = string

  let make name = name
  let compare = String.compare
  let to_string name = name

end


module Local_name : sig

  type t

  val fresh : string -> t
  val compare : t -> t -> int
  val to_string : t -> string

end = struct

  type t = int * string

  let next_id = ref 0

  let fresh (name : string) : t =
    let id = !next_id in
    incr next_id;
    id, name

  let compare (id1, _) (id2, _) =
    Int.compare id1 id2

  let to_string (id, name) =
    Printf.sprintf "%s%i" name id

end


module Item_map = Map.Make (Item_name)
module Local_map = Map.Make (Local_name)


(** Core language *)
module Core = struct

  module Type = struct

    type t =
      | Bool
      | I32

  end

  module rec Item : sig

    type t =
      | Val of Type.t * Expr.t
      | Fun of (Local_name.t * Type.t) Iarray.t * Type.t * Expr.t

  end = Item

  and Expr : sig

    type t =
      | Item of Item_name.t * t Iarray.t option
      | Var of Local_name.t
      | Let of Local_name.t * Type.t * t * t
      | Bool of bool
      | Bool_if of t * t * t
      | I32 of int32
      | Prim of Prim.Op.t * t Iarray.t

    module Value : sig

      type t =
        | Bool of bool
        | I32 of int32

    end

    val eval : Item.t Item_map.t -> Value.t Local_map.t -> t -> Value.t

  end = struct

    include Expr

    module Value = Prim.Value

    let rec eval (items : Item.t Item_map.t) (locals : Value.t Local_map.t) (expr : t) : Value.t =
      match expr with
      | Item (name, args) ->
          begin match Item_map.find name items, args with
          | Item.Val (_, body), None ->
              eval items locals body
          | Item.Fun (names, _, body), Some args ->
              let eval_arg (name, _) arg = name, eval items locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq names) (Iarray.to_seq args) in
              eval items (Local_map.add_seq args locals) body
          | _, _ -> failwith "Expr.eval"
          end
      | Var name -> Local_map.find name locals
      | Let (name, _, def, body) ->
          let def = eval items locals def in
          eval items (Local_map.add name def locals) body
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

  module Program = struct

    type t = Item.t Item_map.t

  end

end


(* An intermediate language where nested let expressions are hoisted to the top
   of expressions. This will be easier easier to translate to web assembly,
   which does not support these kinds of expressions. *)
module Let_hoisted = struct

  module Type = Core.Type

  module rec Expr : sig

    type t =
      | Let of Local_name.t * Type.t * comp * t
      | Bool_if of comp * t * t
      | Comp of comp

    and comp =
      | Item of Item_name.t * comp Iarray.t option
      | Prim of Prim.Op.t * comp Iarray.t
      | Var of Local_name.t
      | Bool of bool
      | I32 of int32

    module Value : sig

      type t =
        | Bool of bool
        | I32 of int32

    end

    val eval : Item.t Item_map.t -> Value.t Local_map.t -> t -> Value.t

  end = struct

    include Expr

    module Value = Prim.Value

    let rec eval (items : Item.t Item_map.t) (locals : Value.t Local_map.t) (expr : t) : Value.t =
      match expr with
      | Let (name, _, def, body) ->
          let def = eval_comp items locals def in
          eval items (Local_map.add name def locals) body
      | Bool_if (expr1, expr2, expr3) ->
          begin match eval_comp items locals expr1 with
          | Value.Bool true -> eval items locals expr2
          | Value.Bool false -> eval items locals expr3
          | _ -> failwith "Expr.eval"
          end
      | Comp expr -> eval_comp items locals expr

    and eval_comp (items : Item.t Item_map.t) (locals : Value.t Local_map.t) (expr : comp) : Value.t =
      match expr with
      | Item (name, args) ->
          begin match Item_map.find name items, args with
          | Item.Fun (names, _, body), Some args ->
              let eval_arg (name, _) arg = name, eval_comp items locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq names) (Iarray.to_seq args) in
              eval items (Local_map.add_seq args locals) body
          | Item.Val (_, body), None -> eval items locals body
          | _ -> failwith "Expr.eval_atom"
          end
      | Prim (op, args) ->
          Prim.Op.app op (Iarray.map (eval_comp items locals) args)
      | Var name -> Local_map.find name locals
      | Bool bool -> Value.Bool bool
      | I32 int -> Value.I32 int

  end

  and Item : sig

    type t =
      | Val of Type.t * Expr.t
      | Fun of (Local_name.t * Type.t) Iarray.t * Type.t * Expr.t

  end = Item

  module Program = struct

    type t = Item.t Item_map.t

  end

end


(** Hoist let expressions to the top of expressions *)
module Hoist_lets : sig

  val translate_expr : Core.Expr.t -> Let_hoisted.Expr.t
  val translate_program : Core.Program.t -> Let_hoisted.Program.t

end = struct

  module Lh = Let_hoisted

  let translate_expr (expr : Core.Expr.t) : Lh.Expr.t =
    let ( let@ ) = ( @@ ) in

    let rec go_expr (expr : Core.Expr.t) (k : Lh.Expr.comp -> Lh.Expr.t) : Lh.Expr.t =
      match expr with
      | Core.Expr.Item (name, None) ->
          k (Lh.Expr.Item (name, None))
      | Core.Expr.Item (name, Some args) ->
          let@ args = go_exprs (Iarray.to_list args) in
          k (Lh.Expr.Item (name, Some (Iarray.of_list args)))
      | Core.Expr.Var name ->
          k (Lh.Expr.Var name)
      | Core.Expr.Let (name, def_ty, def, body) ->
          let@ def = go_expr def in
          let body = go_expr body k in
          Lh.Expr.Let (name, def_ty, def, body)
      | Core.Expr.Bool bool ->
          k (Lh.Expr.Bool bool)
      | Core.Expr.Bool_if (expr1, expr2, expr3) ->
          let@ expr1 = go_expr expr1 in
          let expr2 = go_expr expr2 k in (* FIXME: join point *)
          let expr3 = go_expr expr3 k in (* FIXME: join point *)
          Lh.Expr.Bool_if (expr1, expr2, expr3)
      | Core.Expr.I32 int ->
          k (Lh.Expr.I32 int)
      | Core.Expr.Prim (op, args) ->
          let@ args = go_exprs (Iarray.to_list args) in
          k (Lh.Expr.Prim (op, Iarray.of_list args))

    (* Compile a series of expressions to intermediate definitions *)
    and go_exprs (exprs : Core.Expr.t list) (k : Lh.Expr.comp list -> Lh.Expr.t) : Lh.Expr.t =
      match exprs with
      | [] -> k []
      | expr :: exprs ->
          let@ expr = go_expr expr in
          let@ exprs = go_exprs exprs in
          k (expr :: exprs)
    in

    let@ expr = go_expr expr in
    Lh.Expr.Comp expr

  let translate_item (item : Core.Item.t) : Lh.Item.t =
    match item with
    | Core.Item.Val (ty, def) ->
        Lh.Item.Val (ty, translate_expr def)
    | Core.Item.Fun (params, ty, body) ->
        Lh.Item.Fun (params, ty, translate_expr body)

  let translate_program (program : Core.Program.t) : Lh.Program.t =
    program |> Item_map.map translate_item

end


(** Translate ANF to WAT (WebAssembly Text Format) *)
module Emit_wat : sig

  val pp_program : ?tail_call:bool ->  Let_hoisted.Program.t -> Format.formatter -> unit
    [@@warning "-unused-value-declaration"]

end = struct

  module Lh = Let_hoisted

  type features = {
    tail_call : bool;
  }

  type item_ty_env = Lh.Type.t Item_map.t
  type local_ty_env = Lh.Type.t Local_map.t

  (** Collect the types of local definitions in an expression. This is useful
      pre-declaring locals inside function definitions *)
  let local_def_tys (expr : Lh.Expr.t) : local_ty_env =
    let union = Local_map.union (fun _ ty _ -> Some ty) in
    let rec go_expr expr =
      match expr with
      | Lh.Expr.Let (name, ty, _, body) -> Local_map.add name ty (go_expr body)
      | Lh.Expr.Bool_if (_, expr2, expr3) -> union (go_expr expr2) (go_expr expr3)
      (* All local definitions are floated to the top, so we don't need to
         traverse any further *)
      | Lh.Expr.Comp _ -> Local_map.empty
    in
    go_expr expr

  (** Find the type of an expression. This is useful for finding the return
      type of conditionals. *)
  let ty_of_expr (item_tys : item_ty_env) (local_tys : local_ty_env) (expr : Lh.Expr.t) =
    let rec go_expr expr =
      match expr with
      | Lh.Expr.Let (_, _, _, body) -> go_expr body
      | Lh.Expr.Bool_if (_, expr2, _) -> go_expr expr2
      | Lh.Expr.Comp expr -> go_comp expr
    and go_comp expr =
      match expr with
      | Lh.Expr.Prim (Prim.Op.I32_eq, _) -> Lh.Type.Bool
      | Lh.Expr.Prim (Prim.Op.(I32_add | I32_sub | I32_mul), _) -> Lh.Type.Bool
      | Lh.Expr.Item (name, _) -> Item_map.find name item_tys
      | Lh.Expr.Var name -> Local_map.find name local_tys
      | Lh.Expr.Bool _ -> Lh.Type.Bool
      | Lh.Expr.I32 _ -> Lh.Type.I32
    in
    go_expr expr

  let pp_quoted s ppf = Format.fprintf ppf "\"%s\"" s
  let pp_i32 i ppf = Format.fprintf ppf "%li" i

  let pp_spaced_iter iter exprs ppf =
    Format.pp_print_iter iter (fun ppf expr -> expr ppf) ppf exprs
      ~pp_sep:Format.pp_print_space

  let pp_sexpr_cmd_iter name iter exprs ppf =
    Format.fprintf ppf "@[<hv 2>(%s@ %t)@]" name (pp_spaced_iter iter exprs)

  let pp_spaced = pp_spaced_iter List.iter
  let pp_spaced_seq = pp_spaced_iter Seq.iter
  let pp_sexpr_cmd name = pp_sexpr_cmd_iter name List.iter
  let pp_sexpr_cmd_seq name = pp_sexpr_cmd_iter name Seq.iter

  let pp_item_name (name : Item_name.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%s" (Item_name.to_string name)

  let pp_local_name (name : Local_name.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%s" (Local_name.to_string name)

  let pp_type (ty : Lh.Type.t) (ppf : Format.formatter) =
    match ty with
    | Core.Type.Bool -> Format.fprintf ppf "i32"
    | Core.Type.I32 -> Format.fprintf ppf "i32"

  (** Emit expressions in {{: https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions}
      {e folded} form}. *)
  let pp_expr (fs : features) (item_tys : item_ty_env) (local_tys : local_ty_env) (expr : Lh.Expr.t) (ppf : Format.formatter) =
    let rec go_expr expr =
      match expr with
      | Lh.Expr.Let (name, _, expr, body) ->
          pp_spaced [
            pp_sexpr_cmd "local.set" [
              pp_local_name name;
              go_comp expr;
            ];
            go_expr body;
          ]
      | Lh.Expr.Bool_if (expr1, expr2, expr3) ->
          pp_sexpr_cmd "if" [
            pp_sexpr_cmd "result" [pp_type (ty_of_expr item_tys local_tys expr2)];
            go_comp expr1;
            pp_sexpr_cmd "then" [go_expr expr2];
            pp_sexpr_cmd "else" [go_expr expr3];
          ]
      (* Emit tail-calls if possible *)
      | Lh.Expr.Comp (Item (name, Some args)) when fs.tail_call ->
          pp_sexpr_cmd "return_call" [pp_item_name name; go_args args]
      | Lh.Expr.Comp (Item (name, None)) when fs.tail_call ->
          pp_sexpr_cmd "return_call" [pp_item_name name]
      (* Otherwise emit a return instruction *)
      | Lh.Expr.Comp expr ->
          pp_sexpr_cmd "return" [go_comp expr]
    and go_comp expr =
      match expr with
      | Lh.Expr.Prim (Prim.Op.I32_eq, args) -> pp_sexpr_cmd "i32.eq" [go_args args];
      | Lh.Expr.Prim (Prim.Op.I32_add, args) -> pp_sexpr_cmd "i32.add" [go_args args];
      | Lh.Expr.Prim (Prim.Op.I32_sub, args) -> pp_sexpr_cmd "i32.sub" [go_args args];
      | Lh.Expr.Prim (Prim.Op.I32_mul, args) -> pp_sexpr_cmd "i32.mul" [go_args args];
      | Lh.Expr.Item (name, Some args) -> pp_sexpr_cmd "call" [pp_item_name name; go_args args]
      | Lh.Expr.Item (name, None) -> pp_sexpr_cmd "call" [pp_item_name name]
      | Lh.Expr.Var name -> pp_sexpr_cmd "local.get" [pp_local_name name]
      | Lh.Expr.Bool true -> pp_sexpr_cmd "i32.const" [pp_i32 1l]
      | Lh.Expr.Bool false -> pp_sexpr_cmd "i32.const" [pp_i32 0l]
      | Lh.Expr.I32 int -> pp_sexpr_cmd "i32.const" [pp_i32 int]
    and go_args args =
      pp_spaced_seq (Iarray.to_seq args |> Seq.map go_comp)
    in
    go_expr expr ppf

  let pp_item (fs : features) (item_tys : item_ty_env) (name, item : Lh.(Item_name.t * Item.t)) (ppf : Format.formatter) =
    let pp_fun params ret_ty body =
      let pp_param (name, ty) = pp_sexpr_cmd "param" [pp_local_name name; pp_type ty]
      and pp_local (name, ty) = pp_sexpr_cmd "local" [pp_local_name name; pp_type ty]
      and locals = local_def_tys body in

      (* https://webassembly.github.io/spec/core/text/modules.html#functions *)
      pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
        Seq.singleton (pp_item_name name);
        Seq.singleton (pp_sexpr_cmd "export" [pp_quoted (Item_name.to_string name)]);
        Iarray.to_seq params |> Seq.map pp_param;
        Seq.singleton (pp_sexpr_cmd "result" [pp_type ret_ty]);
        Local_map.to_seq locals |> Seq.map pp_local;
        Seq.singleton (
          let local_tys =
            Seq.append (Iarray.to_seq params) (Local_map.to_seq locals)
            |> Local_map.of_seq
          in
          pp_expr fs item_tys local_tys body
        );
      ])
    in
    match item with
    | Lh.Item.Val (ty, expr) -> pp_fun [||] ty expr ppf (* FIXME: re-evaluation of top-level values *)
    | Lh.Item.Fun (params, ret_ty, body) -> pp_fun params ret_ty body ppf

  let pp_program ?(tail_call = false) (program : Lh.Program.t) : Format.formatter -> unit =
    let fs = { tail_call } in
    let item_tys = program |> Item_map.map @@ function
      | Lh.Item.Val (ty, _) -> ty
      | Lh.Item.Fun (_, ret_ty, _) -> ret_ty
    in
    (* https://webassembly.github.io/spec/core/text/modules.html#text-module *)
    pp_sexpr_cmd_seq "module"
      (Item_map.to_seq program |> Seq.map (pp_item fs item_tys))

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

    let program = Item_map.of_list [
      Item_name.make "test-fact", Item.Val (
        Type.I32,
        Expr.Item (Item_name.make "fact", Some [|Expr.I32 5l|])
      );

      Item_name.make "fact", begin
        let n = Local_name.(fresh "n") in
        Item.Fun (
          [|n, Type.I32|],
          Type.I32,
          Expr.Bool_if (
            Expr.Prim (I32_eq, [|Expr.Var n; Expr.I32 0l|]),
            Expr.I32 1l,
            Expr.Prim (I32_mul, [|
              Expr.Var n;
              Expr.Item (Item_name.make "fact", Some [|
                Expr.Prim (I32_sub, [|Expr.Var n; Expr.I32 1l|]);
              |]);
            |])
          );
        )
      end;

      Item_name.make "ackermann", begin
        let m = Local_name.fresh "m" in
        let n = Local_name.fresh "n" in
        Item.Fun (
          [|m, Type.I32; n, Type.I32|],
          Type.I32,
          Expr.Bool_if (
            Expr.Prim (I32_eq, [|Expr.Var m; Expr.I32 0l|]),
            Expr.Prim (I32_add, [|Expr.Var n; Expr.I32 1l|]),
            Expr.Bool_if (
              Expr.Prim (I32_eq, [|Expr.Var n; Expr.I32 0l|]),
              Expr.Item (Item_name.make "ackermann", Some [|
                Expr.Prim (I32_sub, [|Expr.Var m; Expr.I32 1l|]);
                Expr.I32 1l;
              |]),
              Expr.Item (Item_name.make "ackermann", Some [|
                Expr.Prim (I32_sub, [|Expr.Var m; Expr.I32 1l|]);
                Expr.Item (Item_name.make "ackermann", Some [|
                  Expr.Var m;
                  Expr.Prim (I32_sub, [|Expr.Var n; Expr.I32 1l|]);
                |]);
              |])
            )
          )
        )
      end;

      Item_name.make "is-even", begin
        let n = Local_name.(fresh "n") in
        Item.Fun (
          [|n, Type.I32|],
          Type.Bool,
          Expr.Bool_if (
            Expr.Prim (I32_eq, [|Expr.Var n; Expr.I32 0l|]),
            Expr.Bool true,
            Expr.Item (Item_name.make "is-odd", Some [|
              Expr.Prim (I32_sub, [|Expr.Var n; Expr.I32 1l|]);
            |])
          )
        )
      end;

      Item_name.make "is-odd", begin
        let n = Local_name.(fresh "n") in
        Item.Fun (
          [|n, Type.I32|],
          Type.Bool,
          Expr.Bool_if (
            Expr.Prim (I32_eq, [|Expr.Var n; Expr.I32 0l|]),
            Expr.Bool false,
            Expr.Item (Item_name.make "is-even", Some [|
              Expr.Prim (I32_sub, [|Expr.Var n; Expr.I32 1l|]);
            |])
          )
        )
      end;

    ] in

    let lh_items = Hoist_lets.translate_program program in

    let decode_lh_value lh_value =
      match lh_value with
      | Let_hoisted.Expr.Value.Bool b -> Expr.Value.Bool b
      | Let_hoisted.Expr.Value.I32 i -> Expr.Value.I32 i
    in

    let check_eval expr expected_value =
      let value = Expr.eval program Local_map.empty expr in
      assert (value = expected_value);

      let lh_expr = Hoist_lets.translate_expr expr in
      let lh_value = Let_hoisted.Expr.eval lh_items Local_map.empty lh_expr in
      assert (decode_lh_value lh_value = expected_value);
    in

    test "test-fact" Expr.(fun () -> check_eval (Item (Item_name.make "test-fact", None)) (Value.I32 120l));
    test "ackermann(0, 0)" Expr.(fun () -> check_eval (Item (Item_name.make "ackermann", Some [|I32 0l; I32 0l|])) (Expr.Value.I32 1l));
    test "ackermann(3, 4)" Expr.(fun () -> check_eval (Item (Item_name.make "ackermann", Some [|I32 3l; I32 4l|])) (Value.I32 125l));
    test "is-even(6)" Expr.(fun () -> check_eval (Item (Item_name.make "is-even", Some [|I32 6l|])) (Value.Bool true));
    test "is-odd(6)" Expr.(fun () -> check_eval (Item (Item_name.make "is-odd", Some [|I32 6l|])) (Value.Bool false));

    (* https://taubyte.com/tools/wasm *)
    (* Format.printf "%t" (Emit_wat.pp_program lh_items) *)
    (* Format.printf "%t" (Emit_wat.pp_program ~tail_call:true lh_items) *)

  end

end
