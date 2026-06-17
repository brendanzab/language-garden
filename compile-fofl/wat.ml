(** A minimal representation of the Web Assembly text format *)

module Func_id = Name.Label.Make ()
module Local_id = Name.Label.Make ()

(* https://webassembly.github.io/spec/core/text/types.html *)
type ty =
  | I32

(* https://webassembly.github.io/spec/core/text/instructions.html#text-instrs *)
type instr =
  (* Control instructions *)
  | If of ty * expr * expr
  | Call of Func_id.t
  | Return_call of Func_id.t

  (* Variable instructions *)
  | Local_set of Local_id.t
  | Local_get of Local_id.t

  (* Numeric instructions *)
  | I32_const of int32
  | I32_eq
  | I32_add
  | I32_sub
  | I32_mul
  | I32_neg

(* https://webassembly.github.io/spec/core/text/instructions.html#expressions *)
and expr = instr Iarray.t

(* https://webassembly.github.io/spec/core/text/modules.html#functions *)
type func = {
  name : Func_id.t;
  params : (Local_id.t * ty) Iarray.t;
  result : ty;
  locals : (Local_id.t * ty) Iarray.t;
  body : expr;
}

(* https://webassembly.github.io/spec/core/text/modules.html#exports *)
type export =
  | Func of Func_id.t

(* https://webassembly.github.io/spec/core/text/modules.html#text-module *)
type module_ = {
  exports : (string * export) Iarray.t;
  funcs : func Iarray.t;
}


(** Pretty printing to the web assembly text format *)
module Pretty = struct

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

  let pp_func_id (name : Func_id.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%t" (Func_id.pp name)

  let pp_local_id (id : Local_id.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%t" (Local_id.pp id)

  let pp_type (ty : ty) (ppf : Format.formatter) =
    match ty with
    | I32 -> Format.fprintf ppf "i32"

  let rec pp_instr (instr : instr) =
    match instr with
    | If (ty, expr1, expr2) ->
        pp_sexpr_cmd "if" [
          pp_sexpr_cmd "result" [pp_type ty];
          pp_sexpr_cmd "then" [pp_expr expr1];
          pp_sexpr_cmd "else" [pp_expr expr2];
        ]
    | Call id -> pp_sexpr_cmd "call" [pp_func_id id];
    | Return_call id -> pp_sexpr_cmd "return_call" [pp_func_id id];
    | Local_set id -> pp_sexpr_cmd "local.set" [pp_local_id id];
    | Local_get id -> pp_sexpr_cmd "local.get" [pp_local_id id];
    | I32_const i -> pp_sexpr_cmd "i32.const" [pp_i32 i]
    | I32_eq -> fun ppf -> Format.fprintf ppf "i32.eq"
    | I32_add -> fun ppf -> Format.fprintf ppf "i32.add"
    | I32_sub -> fun ppf -> Format.fprintf ppf "i32.sub"
    | I32_mul -> fun ppf -> Format.fprintf ppf "i32.mul"
    | I32_neg -> fun ppf -> Format.fprintf ppf "i32.neg"

  and pp_expr (expr : expr) (ppf : Format.formatter) =
    Format.pp_print_iter Iarray.iter (Fun.flip pp_instr) ppf expr
      ~pp_sep:Format.pp_print_space

  let pp_func (func : func) =
    let pp_param (id, ty) = pp_sexpr_cmd "param" [pp_local_id id; pp_type ty]
    and pp_local (id, ty) = pp_sexpr_cmd "local" [pp_local_id id; pp_type ty]
    in
    pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
      Seq.singleton (pp_func_id func.name);
      Iarray.to_seq func.params |> Seq.map pp_param;
      Seq.singleton (pp_sexpr_cmd "result" [pp_type func.result]);
      Iarray.to_seq func.locals |> Seq.map pp_local;
      Seq.singleton (pp_expr func.body);
    ])

  let pp_export (name, export : string * export) =
    pp_sexpr_cmd "export" [
      pp_quoted name;
      begin match export with
      | Func id -> pp_sexpr_cmd "func" [pp_func_id id]
      end;
    ]

  let pp_module (module_ : module_) =
    pp_sexpr_cmd_seq "module" (Seq.concat @@ List.to_seq [
      (Iarray.to_seq module_.exports |> Seq.map pp_export);
      (Iarray.to_seq module_.funcs |> Seq.map pp_func);
    ])

end


(** Support for emitting Web Assembly modules. Higher-rank types are used to
    ensure that contexts do not escape their region, but this could be removed
    as a simplification.

    This was inspired by Andreas Rossberg’s Emit module for Waml:

    - https://github.com/WebAssembly/gc/blob/waml/proposals/gc/waml/src/emit.mli
    - https://github.com/WebAssembly/gc/blob/waml/proposals/gc/waml/src/emit.ml
*)
module Emit : sig

  (** Context for emitting expressions *)
  module Expr_ctx : sig

    type 'a t

    (* NOTE: Can be removed when we upgrade to OCaml 5.5 *)
    type region = {
      run : 'a. 'a t -> unit;
    }

    val emit_param : 'a t -> string -> ty -> Local_id.t
    val emit_local : 'a t -> string -> ty -> Local_id.t
    val emit_instr : 'a t -> instr -> unit

    val emit_expr : 'a t -> region -> expr
    (** Emit a sub-expression. Any parameters and locals emitted within the
        sub-expression will be added to the root expression. *)

  end

  (** Context for emitting modules *)
  module Module_ctx : sig

    type 'a t

    (* NOTE: Can be removed when we upgrade to OCaml 5.5 *)
    type region = {
      run : 'a. 'a t -> unit;
    }

    val emit_export : 'a t -> string -> export -> unit

    val emit_func_deferred : 'a t -> string -> Func_id.t * ('a t -> ty -> Expr_ctx.region -> unit)
    (** Begin emitting a new function.

        The function's identifier will be returned, along with closure that
        allows us to finish defining the function later on. This is useful for
        handling mutual recursion during compilation.
    *)

    val build : region -> module_

  end

end = struct

  module Local_supply = Name.Label.Supply (Local_id)
  module Func_supply = Name.Label.Supply (Func_id)

  (* NOTE: This can be replaced with Dynarray.to_iarray when we move to OCaml 5.5
    https://github.com/ocaml/ocaml/commit/5a7ab30d9e6241c17c8f28a826b96459ca5cdd95 *)
  let make_iarray xs =
    Iarray.init (Dynarray.length xs) (Dynarray.get xs)

  module Expr_ctx = struct

    type 'a t = {
      local_supply : Local_supply.t;
      params : (Local_id.t * ty) Dynarray.t;
      locals : (Local_id.t * ty) Dynarray.t;
      instrs : instr Dynarray.t;
    }

    type region = {
      run : 'a. 'a t -> unit;
    }

    let create () : 'a t = {
      local_supply = Local_supply.create ();
      params = Dynarray.create ();
      locals = Dynarray.create ();
      instrs = Dynarray.create ();
    }

    let emit_param (ctx : 'a t) (name : string) (ty : ty) =
      let id = Local_supply.fresh ctx.local_supply name in
      Dynarray.add_last ctx.params (id, ty);
      id

    let emit_local (ctx : 'a t) (name : string) (ty : ty) =
      let id = Local_supply.fresh ctx.local_supply name in
      Dynarray.add_last ctx.locals (id, ty);
      id

    let emit_instr (ctx : 'a t) (instr : instr) =
      Dynarray.add_last ctx.instrs instr

    let emit_expr (ctx : 'a t) (body : region) =
      let ctx = { ctx with instrs = Dynarray.create () } in
      body.run ctx;
      make_iarray ctx.instrs

    let build (name : Func_id.t) (result : ty) (body : region) : func =
      let ctx' = create () in
      body.run ctx';

      let params = make_iarray ctx'.params in
      let locals = make_iarray ctx'.locals in
      let body = make_iarray ctx'.instrs in

      { name; params; result; locals; body }

  end

  module Module_ctx = struct

    type 'a t = {
      func_supply : Func_supply.t;
      exports : (string * export) Dynarray.t;
      funcs : func option Dynarray.t;
    }

    type region = { run : 'a. 'a t -> unit }

    let create () : 'a t = {
      func_supply = Func_supply.create ();
      exports = Dynarray.create ();
      funcs = Dynarray.create ();
    }

    let emit_export (ctx : 'a t) (name : string) (export : export) =
      Dynarray.add_last ctx.exports (name, export)

    let emit_func_deferred (ctx : 'a t) (name : string) : Func_id.t * ('a t -> ty -> Expr_ctx.region -> unit) =
      let index = Dynarray.length ctx.funcs in
      let id = Func_supply.fresh ctx.func_supply name in
      Dynarray.add_last ctx.funcs None;
      let define_func ctx result body =
        Dynarray.set ctx.funcs index (Some (Expr_ctx.build id result body))
      in
      id, define_func

    let build (body : region) : module_ =
      let ctx = create () in
      body.run ctx;
      {
        exports = make_iarray ctx.exports;
        funcs =
          Iarray.init (Dynarray.length ctx.funcs)
            (fun n -> Dynarray.get ctx.funcs n |> Option.get);
      }

  end

end
