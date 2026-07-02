(** A minimal representation of LLVM IR.

    I found that the process of defining this AST helped me to gain a better
    understanding of the surface area of the LLVM language. That said, in a
    production compiler it might be better to use bindings that manipulate
    LLVM’s in-memory data structures directly, for example Ocaml’s
    {{: https://ocaml.org/p/llvm} llvm} package.

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
  (* ... *)

(** Operands *)
type opr =
  | I1 of bool
  | I32 of Int32.t
  (* ... *)
  | Global of Global_id.t
  | Local of Local_id.t

(** Comparison condition codes *)
type icmp_cond =    (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Eq              (* equal *)
  (* ... *)

(** Instructions *)
type instr =
  | Add of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#add-instruction *)
  | Sub of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#sub-instruction *)
  | Mul of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#mul-instruction *)
  | Icmp of icmp_cond * ty * opr * opr      (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Phi of ty * (opr * Label.t) Iarray.t    (* https://llvm.org/docs/LangRef.html#phi-instruction *)
  | Call of ty * opr * (ty * opr) Iarray.t  (* https://llvm.org/docs/LangRef.html#call-instruction *)
  (* ... *)

(** Terminator instructions *)
type term =                                 (* Terminator instructions  https://llvm.org/docs/LangRef.html#terminator-instructions *)
  | Br of Label.t                           (* Unconditional branch     https://llvm.org/docs/LangRef.html#i-br *)
  | Br_i1 of opr * Label.t * Label.t        (* Conditional branch       https://llvm.org/docs/LangRef.html#i-br *)
  | Ret of ty * opr                         (* Return instruction       https://llvm.org/docs/LangRef.html#ret-instruction *)
  (* ... *)

(** Basic blocks *)
type block =
  | Instr of Local_id.t * instr * block
  | Term of term

(* Control flow graph of a function *)
type cfg = {
  entry : Label.t * block;
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

(** Output the AST in LLVM’s human readable assembly language representation *)
module Output_ll : sig

  val pp_module : module_ -> Format.formatter -> unit
  val pp_block : block -> Format.formatter -> unit
  val pp_ty : ty -> Format.formatter -> unit

  val pp_global_id : Global_id.t -> Format.formatter -> unit
  val pp_local_id : Local_id.t -> Format.formatter -> unit

end = struct

  let pp_comma_sep ppf () = Format.fprintf ppf ",@ "
  let pp_iarray ?pp_sep f list ppf =
    Format.pp_print_iter Iarray.iter (Fun.flip f) ppf list ?pp_sep

  let pp_global_id (id : Global_id.t) = Format.dprintf "%s%t" "@" (Global_id.pp id)
  let pp_local_id (id : Local_id.t) = Format.dprintf "%s%t" "%" (Local_id.pp id)
  let pp_label (id : Label.t) = Format.dprintf "%s%t" "%" (Label.pp id)

  let pp_ty (ty : ty) =
    match ty with
    | I1 -> Format.dprintf "i1"
    | I32 -> Format.dprintf "i32"

  let pp_opr (opr : opr) =
    match opr with
    | I1 true -> Format.dprintf "true"
    | I1 false -> Format.dprintf "false"
    | I32 int -> Format.dprintf "%li" int
    | Global id -> pp_global_id id
    | Local id -> pp_local_id id

  let pp_instr (instr : instr) =
    let pp_binop_instr (name, ty, opr1, opr2) =
      Format.dprintf "@[%s@ %t@ %t,@ %t@]" name (pp_ty ty) (pp_opr opr1) (pp_opr opr2)
    and pp_pred (opr, label) = Format.dprintf "[@[%t,@ %t@]]" (pp_opr opr) (pp_label label)
    and pp_arg (ty, opr) = Format.dprintf "@[%t@ %t@]" (pp_ty ty) (pp_opr opr)
    in
    match instr with
    | Add (ty, opr1, opr2) -> pp_binop_instr ("add", ty, opr1, opr2)
    | Sub (ty, opr1, opr2) -> pp_binop_instr ("sub", ty, opr1, opr2)
    | Mul (ty, opr1, opr2) -> pp_binop_instr ("mul", ty, opr1, opr2)
    | Icmp (cond, ty, opr1, opr2) ->
        let cond =
          match cond with
          | Eq -> "eq"
        in
        Format.dprintf "@[icmp@ %s@ %t@ %t,@ %t@]" cond (pp_ty ty) (pp_opr opr1) (pp_opr opr2)
    | Phi (ty, preds) ->
        Format.dprintf "@[<hv 2>@[phi@ %t@]@ %t@]"
          (pp_ty ty)
          (preds |> pp_iarray pp_pred ~pp_sep:pp_comma_sep)
    | Call (ty, fn, args) ->
        Format.dprintf "@[call@ %t@ %t(%t)@]"
          (pp_ty ty)
          (pp_opr fn)
          (args |> pp_iarray pp_arg ~pp_sep:pp_comma_sep)

  let pp_term (term : term) =
    match term with
    | Br dest -> Format.dprintf "@[  @[br@ label@ %t@]@]" (pp_label dest)
    | Br_i1 (cond, if_true, if_false) ->
        Format.dprintf "@[  @[br@ i1@ %t,@ label@ %t,@ label@ %t@]@]"
          (pp_opr cond) (pp_label if_true) (pp_label if_false)
    | Ret (ty, opr) -> Format.dprintf "@[  @[ret@ %t@ %t@]@]" (pp_ty ty) (pp_opr opr)

  let rec pp_block (block : block) =
    match block with
    | Instr (id, instr, block) ->
        Format.dprintf "@[  @[<2>@[%t@ =@]@ %t@]@]@,%t"
          (pp_local_id id)
          (pp_instr instr)
          (pp_block block)
    | Term term ->
        pp_term term

  let pp_cfg ({ entry; blocks; } : cfg) =
    let pp_labelled_block (label, block) =
      Format.dprintf "%t:@,%t" (Label.pp label) (pp_block block)
    in
    if Iarray.length blocks = 0 then
      pp_labelled_block entry
    else
      Format.dprintf "%t@ %t"
        (pp_labelled_block entry)
        (blocks |> pp_iarray pp_labelled_block)

  let pp_fun (id, { result_ty; params; cfg } : Global_id.t * fun_) =
    let pp_param (ty, id) =
      Format.dprintf "@[%t@ %t@]" (pp_ty ty) (pp_local_id id)
    in
    Format.dprintf "@[<v>@[define@ %t@ %t(%t)@ {@]@ %t@ }@]@."
      (pp_ty result_ty)
      (pp_global_id id)
      (pp_iarray pp_param params ~pp_sep:pp_comma_sep)
      (pp_cfg cfg)

  let pp_module ({ funs } : module_) =
    funs |> pp_iarray pp_fun ~pp_sep:Format.pp_print_newline

end

(** Output the AST in Graphviz’s {{: https://graphviz.org/doc/info/lang.html}
    DOT language}. This can help with visualising control flow graphs. *)
module Output_dot = struct

  let pp_block (fun_id : Global_id.t) (label, block : Label.t * block) (out : Out_channel.t) = begin
    let rec outgoing_labels block =
      match block with
      | Instr (_, _, block) -> outgoing_labels block
      | Term (Br label) -> [label]
      | Term (Br_i1 (_, label1, label2)) -> [label1; label2]
      | Term (Ret (_, _)) -> []
    in

    let label_string label =
      Format.asprintf "\"%t.%t\"" (Global_id.pp fun_id) (Label.pp label)
    in

    let block_text =
      Format.asprintf "@[<v>%t@]" (Output_ll.pp_block block)
    in

    (* Basic block *)
    Printf.fprintf out "\n";
    Printf.fprintf out "    %s [\n" (label_string label);
    Printf.fprintf out "      label=<<table color=\"black\" border=\"0\" cellborder=\"0\" cellpadding=\"3\">\n";
    Printf.fprintf out "        <th><td align=\"left\" border=\"1\" sides=\"b\">%s:</td></th>\n" (Label.to_string label);
    String.split_on_char '\n' block_text |> List.iter (Printf.fprintf out "        <tr><td align=\"left\">%s</td></tr>\n");
    Printf.fprintf out "      </table>>\n";
    Printf.fprintf out "    ];\n";

    (* Control flow edges *)
    outgoing_labels block |> List.iter begin fun end_label ->
      Printf.fprintf out "    %s -> %s;\n" (label_string label) (label_string end_label)
    end;
  end

  let pp_module ({ funs } : module_) (out : Out_channel.t) = begin
    Printf.fprintf out "digraph llvm_ir {\n";
    Printf.fprintf out "  graph [\n";
    Printf.fprintf out "    fontname=\"Monaco, monospace\";\n";
    Printf.fprintf out "    color=\"none\";\n";
    Printf.fprintf out "    fillcolor=\"gainsboro\";\n";
    Printf.fprintf out "    style=\"filled, rounded\";\n";
    Printf.fprintf out "  ]\n";
    Printf.fprintf out "\n";
    Printf.fprintf out "  node [\n";
    Printf.fprintf out "    fontname=\"Monaco, monospace\";\n";
    Printf.fprintf out "    shape=\"box\";\n";
    Printf.fprintf out "    color=\"none\";\n";
    Printf.fprintf out "    fillcolor=\"white\";\n";
    Printf.fprintf out "    style=\"filled, rounded\";\n";
    Printf.fprintf out "  ]\n";
    Printf.fprintf out "\n";

    (* Functions *)
    funs |> Iarray.iter begin fun (id, { result_ty; params; cfg }) ->
      Printf.fprintf out "  subgraph \"%s\" {\n" (Global_id.to_string id);

      (* Function signature *)
      Printf.fprintf out "    label=<<table color=\"black\" border=\"0\" cellborder=\"0\" cellpadding=\"3\">\n";
      Printf.fprintf out "      <th><td align=\"left\" border=\"1\" sides=\"b\">\n";
      Printf.fprintf out "        %s %s(%t)\n"
        (Output_ll.pp_ty result_ty |> Format.asprintf "%t")
        (Output_ll.pp_global_id id |> Format.asprintf "%t")
        (fun out ->
          params |> Iarray.iteri begin fun i (ty, id) ->
            if i <> 0 then Printf.fprintf out ", ";
            Printf.fprintf out "%s %s"
              (Output_ll.pp_ty ty |> Format.asprintf "%t")
              (Output_ll.pp_local_id id |> Format.asprintf "%t");
          end);
      Printf.fprintf out "      </td></th>\n";
      Printf.fprintf out "    </table>>;\n";
      Printf.fprintf out "    cluster=true;\n";

      (* Control flow graph *)
      pp_block id cfg.entry out;
      cfg.blocks |> Iarray.iter begin fun (label, block) ->
        pp_block id (label, block) out;
      end;

      Printf.fprintf out "  }\n";
    end;

    Printf.fprintf out "}\n";
  end

end
