(** A minimal representation of the Web Assembly text format *)

module Type_id = Name.Make ()
module Func_id = Name.Make ()
module Local_id = Name.Make ()

(* https://webassembly.github.io/spec/core/text/types.html#heap-types *)
type heap_type =
  | Func
  | Id of Type_id.t

(* https://webassembly.github.io/spec/core/text/types.html#reference-types *)
type ref_type = [`Null] option * heap_type

(* https://webassembly.github.io/spec/core/text/types.html#value-types *)
type value_type =
  | I32
  | Ref of ref_type

(* https://webassembly.github.io/spec/core/text/types.html#composite-types *)
type func_type = {
  params : value_type Iarray.t;
  results : value_type Iarray.t;
}

(* https://webassembly.github.io/spec/core/text/types.html#composite-types *)
type comp_type =
  | Func of func_type

(* https://webassembly.github.io/spec/core/text/types.html#text-rectype *)
type rec_type =
  | Type_def of Type_id.t * comp_type

(* https://webassembly.github.io/spec/core/text/instructions.html#text-instrs *)
type instr =
  (* Control instructions *)
  | If of value_type * expr * expr
  | Call of Func_id.t
  | Call_ref of Type_id.t
  | Return_call of Func_id.t
  | Return_call_ref of Type_id.t

  (* Variable instructions *)
  | Local_set of Local_id.t
  | Local_get of Local_id.t

  (* Reference instructions *)
  | Ref_func of Func_id.t

  (* Numeric instructions *)
  | I32_const of int32
  | I32_eq
  | I32_add
  | I32_sub
  | I32_mul

(* https://webassembly.github.io/spec/core/text/instructions.html#expressions *)
and expr = instr Iarray.t

(* https://webassembly.github.io/spec/core/text/types.html#text-comptype *)
type param = Local_id.t * value_type

(* https://webassembly.github.io/spec/core/text/modules.html#text-local *)
type local = Local_id.t * value_type

(* https://webassembly.github.io/spec/core/text/modules.html#functions *)
type func = {
  id : Func_id.t;
  params : param Iarray.t;
  results : value_type Iarray.t;
  locals : local Iarray.t;
  body : expr;
}

(* https://webassembly.github.io/spec/core/text/modules.html#element-segments *)
type elem_mode =
  | Declare

(* https://webassembly.github.io/spec/core/text/modules.html#text-elemlist *)
type elem_list =
  | Expr of ref_type * expr Iarray.t
  | Func of Func_id.t Iarray.t

(* https://webassembly.github.io/spec/core/text/modules.html#element-segments *)
type elem = elem_mode * elem_list

(* https://webassembly.github.io/spec/core/text/modules.html#text-externidx *)
type extern =
  | Func of Func_id.t

(* https://webassembly.github.io/spec/core/text/modules.html#exports *)
type export = string * extern

(* https://webassembly.github.io/spec/core/text/modules.html#text-module *)
type module_ = {
  exports : export Iarray.t;
  types : rec_type Iarray.t;
  elems : elem Iarray.t;
  funcs : func Iarray.t;
}


(** Output the AST to the WebAssembly text format (WAT) *)
module Output_wat = struct

  let pp_quoted s ppf = Format.fprintf ppf "\"%s\"" s
  let pp_i32 i ppf = Format.fprintf ppf "%li" i

  let pp_spaced_iter iter exprs ppf =
    Format.pp_print_iter iter (fun ppf expr -> expr ppf) ppf exprs
      ~pp_sep:Format.pp_print_space

  let pp_sexpr_cmd_iter name iter exprs =
    Format.dprintf "@[<hv 2>(%s@ %t)@]" name (pp_spaced_iter iter exprs)

  let pp_sexpr_cmd name = pp_sexpr_cmd_iter name List.iter
  let pp_sexpr_cmd_seq name = pp_sexpr_cmd_iter name Seq.iter

  let pp_type_id (id : Type_id.t) = Format.dprintf "$%t" (Type_id.pp id)
  let pp_func_id (id : Func_id.t) = Format.dprintf "$%t" (Func_id.pp id)
  let pp_local_id (id : Local_id.t) = Format.dprintf "$%t" (Local_id.pp id)

  let pp_heap_type (ty : heap_type) =
    match ty with
    | Func -> Format.dprintf "func"
    | Id id -> pp_type_id id

  let pp_ref_type (ty : ref_type) =
    match ty with
    | Some `Null, heap_ty -> pp_sexpr_cmd "ref" [Format.dprintf "null"; pp_heap_type heap_ty]
    | None, heap_ty -> pp_sexpr_cmd "ref" [pp_heap_type heap_ty]

  let pp_value_type (ty : value_type) =
    match ty with
    | I32 -> Format.dprintf "i32"
    | Ref ty -> pp_ref_type ty

  let pp_comp_type (ty : comp_type) =
    match ty with
    | Func { params; results } ->
        pp_sexpr_cmd "func" [
          pp_sexpr_cmd_seq "param" (Iarray.to_seq params |> Seq.map pp_value_type);
          pp_sexpr_cmd_seq "result" (Iarray.to_seq params |> Seq.map pp_value_type);
        ]

  let pp_comp_type (ty : comp_type) =
    match ty with
    | Func { params; results } ->
        pp_sexpr_cmd "func" [
          pp_sexpr_cmd_seq "param" (Iarray.to_seq params |> Seq.map pp_value_type);
          pp_sexpr_cmd_seq "result" (Iarray.to_seq params |> Seq.map pp_value_type);
        ]

  let pp_rec_type (ty : rec_type) =
    match ty with
    | Type_def (id, ty) ->
        pp_sexpr_cmd "type" [
          pp_type_id id;
          pp_comp_type ty;
        ]

  let rec pp_instr (instr : instr) =
    match instr with
    | If (ty, expr1, expr2) ->
        pp_sexpr_cmd "if" [
          pp_sexpr_cmd "result" [pp_value_type ty];
          pp_sexpr_cmd "then" [pp_expr expr1];
          pp_sexpr_cmd "else" [pp_expr expr2];
        ]
    | Call id -> pp_sexpr_cmd "call" [pp_func_id id]
    | Call_ref id -> pp_sexpr_cmd "call_ref" [pp_type_id id]
    | Return_call id -> pp_sexpr_cmd "return_call" [pp_func_id id]
    | Return_call_ref id -> pp_sexpr_cmd "return_call_ref" [pp_type_id id]
    | Local_set id -> pp_sexpr_cmd "local.set" [pp_local_id id]
    | Local_get id -> pp_sexpr_cmd "local.get" [pp_local_id id]
    | Ref_func id -> pp_sexpr_cmd "ref.func" [pp_func_id id]
    | I32_const i -> pp_sexpr_cmd "i32.const" [pp_i32 i]
    | I32_eq -> Format.dprintf "i32.eq"
    | I32_add -> Format.dprintf "i32.add"
    | I32_sub -> Format.dprintf "i32.sub"
    | I32_mul -> Format.dprintf "i32.mul"

  and pp_expr (expr : expr) (ppf : Format.formatter) =
    Format.pp_print_iter Iarray.iter (Fun.flip pp_instr) ppf expr
      ~pp_sep:Format.pp_print_space

  let pp_func (func : func) =
    let pp_param (id, ty) = pp_sexpr_cmd "param" [pp_local_id id; pp_value_type ty] in
    let pp_result ty = pp_sexpr_cmd "result" [pp_value_type ty] in
    let pp_local (id, ty) = pp_sexpr_cmd "local" [pp_local_id id; pp_value_type ty] in
    pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
      Seq.singleton (pp_func_id func.id);
      Iarray.to_seq func.params |> Seq.map pp_param;
      Iarray.to_seq func.results |> Seq.map pp_result;
      Iarray.to_seq func.locals |> Seq.map pp_local;
      Seq.singleton (pp_expr func.body);
    ])

  let pp_elem_mode (mode: elem_mode) =
    match mode with
    | Declare -> Format.dprintf "declare"

  let pp_elem (elem : elem) =
    match elem with
    | mode, Expr (ty, exprs) ->
        Format.dprintf "@[<hv 2>(@[%t@]@ %t)@]"
          (Format.dprintf "elem@ %t@ %t" (pp_elem_mode mode) (pp_ref_type ty))
          (pp_spaced_iter Seq.iter (Iarray.to_seq exprs |> Seq.map pp_expr))
    | mode, Func ids ->
        Format.dprintf "@[<hv 2>(@[%t@]@ %t)@]"
          (Format.dprintf "elem@ %t@ func" (pp_elem_mode mode))
          (pp_spaced_iter Seq.iter (Iarray.to_seq ids |> Seq.map pp_func_id))

  let pp_export (name, extern : export) =
    pp_sexpr_cmd "export" [
      pp_quoted name;
      begin match extern with
      | Func id -> pp_sexpr_cmd "func" [pp_func_id id]
      end;
    ]

  let pp_module (module_ : module_) =
    pp_sexpr_cmd_seq "module" (Seq.concat @@ List.to_seq [
      Iarray.to_seq module_.exports |> Seq.map pp_export;
      Iarray.to_seq module_.types |> Seq.map pp_rec_type;
      Iarray.to_seq module_.elems |> Seq.map pp_elem;
      Iarray.to_seq module_.funcs |> Seq.map pp_func;
    ])

end
