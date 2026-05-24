(** A minimal representation of the web assembly text format *)

module Func_id = Name.Fresh.Make ()
module Local_id = Name.Fresh.Make ()

type ty =
  | I32

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

and expr = instr list

(* https://webassembly.github.io/spec/core/text/modules.html#functions *)
type func = {
  name : Func_id.t;
  export : string;
  params : (Local_id.t * ty) list;
  result : ty;
  locals : (Local_id.t * ty) list;
  body : expr;
}

type module_ = {
  funcs : func list;
}

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
    Format.fprintf ppf "$%s" (Func_id.to_string name)

  let pp_local_id (id : Local_id.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%s" (Local_id.to_string id)

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
    Format.pp_print_list (Fun.flip pp_instr) ppf expr
      ~pp_sep:Format.pp_print_space

  let pp_func (func : func) =
    let pp_param (id, ty) = pp_sexpr_cmd "param" [pp_local_id id; pp_type ty]
    and pp_local (id, ty) = pp_sexpr_cmd "local" [pp_local_id id; pp_type ty]
    in
    pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
      Seq.singleton (pp_func_id func.name);
      Seq.singleton (pp_sexpr_cmd "export" [pp_quoted func.export]);
      List.to_seq func.params |> Seq.map pp_param;
      Seq.singleton (pp_sexpr_cmd "result" [pp_type func.result]);
      List.to_seq func.locals |> Seq.map pp_local;
      Seq.singleton (pp_expr func.body);
    ])

  let pp_module (module_ : module_) =
    pp_sexpr_cmd_seq "module"
      (List.to_seq module_.funcs |> Seq.map pp_func)

end
