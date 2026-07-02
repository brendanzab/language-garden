(** A minimal representation of the Web Assembly text format *)

module Func_id = Name.Make ()
module Local_id = Name.Make ()

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
  id : Func_id.t;
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


(** Output the AST to the WebAssembly text format (WAT) *)
module Output_wat = struct

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
    | I32_eq -> Format.dprintf "i32.eq"
    | I32_add -> Format.dprintf "i32.add"
    | I32_sub -> Format.dprintf "i32.sub"
    | I32_mul -> Format.dprintf "i32.mul"
    | I32_neg -> Format.dprintf "i32.neg"

  and pp_expr (expr : expr) (ppf : Format.formatter) =
    Format.pp_print_iter Iarray.iter (Fun.flip pp_instr) ppf expr
      ~pp_sep:Format.pp_print_space

  let pp_func (func : func) =
    let pp_param (id, ty) = pp_sexpr_cmd "param" [pp_local_id id; pp_type ty]
    and pp_local (id, ty) = pp_sexpr_cmd "local" [pp_local_id id; pp_type ty]
    in
    pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
      Seq.singleton (pp_func_id func.id);
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
