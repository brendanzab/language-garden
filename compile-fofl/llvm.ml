(** A minimal representation of LLVM IR

    - {{: https://llvm.org/docs/LangRef.html} LLVM Language Reference Manual}
    - {{: https://web.archive.org/web/20210503233207/https://www.cis.upenn.edu/~cis341/20sp/hw/hw03/llvmlite.shtml}
      LLVMLite Documentation}
    - {{: https://hackage.haskell.org/package/llvm-hs-pure} llvm-hs-pure: Pure
      Haskell LLVM functionality (no FFI)}
*)

module Global_id = Name.Make ()
module Local_id = Name.Make ()
module Label = Name.Make ()

type ty =
  | I1
  | I32

(** Operands *)
type opr =
  | I1 of bool
  | I32 of Int32.t
  | Global of Global_id.t
  | Local of Local_id.t

(** Comparison condition codes *)
type icmp_cond =    (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Eq              (* equal *)

(** Instructions *)
type instr =
  | Add of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#add-instruction *)
  | Sub of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#sub-instruction *)
  | Mul of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#mul-instruction *)
  | Icmp of icmp_cond * ty * opr * opr      (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Phi of ty * (opr * Label.t) Iarray.t    (* https://llvm.org/docs/LangRef.html#phi-instruction *)
  | Call of ty * opr * (ty * opr) Iarray.t  (* https://llvm.org/docs/LangRef.html#call-instruction *)

(** Terminator instructions *)
type term =                                 (* Terminator instructions  https://llvm.org/docs/LangRef.html#terminator-instructions *)
  | Br of Label.t                           (* Unconditional branch     https://llvm.org/docs/LangRef.html#i-br *)
  | Br_i1 of opr * Label.t * Label.t        (* Conditional branch       https://llvm.org/docs/LangRef.html#i-br *)
  | Ret of ty * opr                         (* Return instruction       https://llvm.org/docs/LangRef.html#ret-instruction *)

(** Basic blocks *)
type block =
  | Instr of Local_id.t * instr * block
  | Term of term

(* Control flow graph of a function *)
type cfg = {
  entry : block;
  blocks : (Label.t * block) Iarray.t;
}

(** Function definitions *)
type fun_ = {                               (* https://llvm.org/docs/LangRef.html#functions *)
  result_ty : ty;
  params : (ty * Local_id.t) Iarray.t;
  cfg : cfg;
}

(** Modules *)
type module_ = {                            (* https://llvm.org/docs/LangRef.html#id2033 *)
  funs : (Global_id.t * fun_) Iarray.t;
}

module Pretty : sig

  val pp_module : module_ -> Format.formatter -> unit

  val pp_global_id : Global_id.t -> Format.formatter -> unit
  val pp_local_id : Local_id.t -> Format.formatter -> unit

end = struct

  let pp_comma_sep ppf () = Format.fprintf ppf ",@ "
  let pp_iarray ?pp_sep f list ppf =
    Format.pp_print_iter Iarray.iter (Fun.flip f) ppf list ?pp_sep

  let pp_global_id (id : Global_id.t) (ppf : Format.formatter) =
    Format.fprintf ppf "%s%t" "@" (Global_id.pp id)

  let pp_local_id (id : Local_id.t) (ppf : Format.formatter) =
    Format.fprintf ppf "%s%t" "%" (Local_id.pp id)

  let pp_label (id : Label.t) (ppf : Format.formatter) =
    Format.fprintf ppf "%s%t" "%" (Label.pp id)

  let pp_ty (ty : ty) (ppf : Format.formatter) =
    match ty with
    | I1 -> Format.fprintf ppf "i1"
    | I32 -> Format.fprintf ppf "i32"

  let pp_opr (opr : opr) (ppf : Format.formatter) =
    match opr with
    | I1 true -> Format.fprintf ppf "true"
    | I1 false -> Format.fprintf ppf "false"
    | I32 int -> Format.fprintf ppf "%li" int
    | Global id -> pp_global_id id ppf
    | Local id -> pp_local_id id ppf

  let pp_instr (instr : instr) (ppf : Format.formatter) =
    let pp_binop_instr (name, ty, opr1, opr2) ppf =
      Format.fprintf ppf "@[%s@ %t@ %t,@ %t@]" name (pp_ty ty) (pp_opr opr1) (pp_opr opr2)
    and pp_pred (opr, label) ppf = Format.fprintf ppf "[@[%t,@ %t@]]" (pp_opr opr) (pp_label label)
    and pp_arg (ty, opr) ppf = Format.fprintf ppf "@[%t@ %t@]" (pp_ty ty) (pp_opr opr)
    in
    match instr with
    | Add (ty, opr1, opr2) -> pp_binop_instr ("add", ty, opr1, opr2) ppf
    | Sub (ty, opr1, opr2) -> pp_binop_instr ("sub", ty, opr1, opr2) ppf
    | Mul (ty, opr1, opr2) -> pp_binop_instr ("mul", ty, opr1, opr2) ppf
    | Icmp (cond, ty, opr1, opr2) ->
        let cond =
          match cond with
          | Eq -> "eq"
        in
        Format.fprintf ppf "@[icmp@ %s@ %t@ %t,@ %t@]" cond (pp_ty ty) (pp_opr opr1) (pp_opr opr2)
    | Phi (ty, preds) ->
        Format.fprintf ppf "@[phi@ %t@ %t@]"
          (pp_ty ty)
          (preds |> pp_iarray pp_pred ~pp_sep:pp_comma_sep)
    | Call (ty, fn, args) ->
        Format.fprintf ppf "@[call@ %t@ %t(%t)@]"
          (pp_ty ty)
          (pp_opr fn)
          (args |> pp_iarray pp_arg ~pp_sep:pp_comma_sep)

  let pp_term (term : term) (ppf : Format.formatter) =
    match term with
    | Br dest -> Format.fprintf ppf "@[  @[br@ label@ %t@]@]" (pp_label dest)
    | Br_i1 (cond, if_true, if_false) ->
        Format.fprintf ppf "@[  @[br@ i1@ %t,@ label@ %t,@ label@ %t@]@]"
          (pp_opr cond) (pp_label if_true) (pp_label if_false)
    | Ret (ty, opr) -> Format.fprintf ppf "@[  @[ret@ %t@ %t@]@]" (pp_ty ty) (pp_opr opr)

  let rec pp_block (block : block) (ppf : Format.formatter) =
    match block with
    | Instr (id, instr, block) ->
        Format.fprintf ppf "@[  @[<2>@[%t@ =@]@ %t@]@]@,%t"
          (pp_local_id id)
          (pp_instr instr)
          (pp_block block)
    | Term term ->
        pp_term term ppf

  let pp_cfg ({ entry; blocks; } : cfg) (ppf : Format.formatter) =
    if Iarray.length blocks = 0 then
      pp_block entry ppf
    else
      Format.fprintf ppf "%t@ %t"
        (pp_block entry)
        (blocks |> pp_iarray
          (fun (label, block) ppf ->
            Format.fprintf ppf "%t:@,%t" (Label.pp label) (pp_block block)))

  let pp_fun (id, { result_ty; params; cfg } : Global_id.t * fun_) (ppf : Format.formatter) =
    let pp_param (ty, id) ppf =
      Format.fprintf ppf "@[%t@ %t@]" (pp_ty ty) (pp_local_id id)
    in
    Format.fprintf ppf "@[<v>@[define@ %t@ %t(%t)@ {@]@ %t@ }@]@."
      (pp_ty result_ty)
      (pp_global_id id)
      (pp_iarray pp_param params ~pp_sep:pp_comma_sep)
      (pp_cfg cfg)

  let pp_module ({ funs } : module_) : Format.formatter -> unit =
    funs |> pp_iarray pp_fun ~pp_sep:Format.pp_print_newline

end
