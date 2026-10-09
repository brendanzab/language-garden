(** A minimal representation of LLVM IR.

    I found that the process of defining this AST helped me to gain a better
    understanding of the surface area of the LLVM language. That said, in a
    production compiler it might be better to use bindings that manipulate
    LLVM’s in-memory data structures directly, for example Ocaml’s
    {{: https://ocaml.org/p/llvm} llvm} package.

    - {{: https://llvm.org/docs/LangRef.html} LLVM Language Reference Manual}
    - {{: https://github.com/llir/grammar} EBNF grammar of LLVM IR assembly}
    - {{: https://web.archive.org/web/20210503233207/https://www.cis.upenn.edu/~cis341/20sp/hw/hw03/llvmlite.shtml}
      LLVMLite Documentation}
    - {{: https://hackage.haskell.org/package/llvm-hs-pure} llvm-hs-pure: Pure
      Haskell LLVM functionality (no FFI)}
*)

module Global_id = Name.Make ()
module Local_id = Name.Make ()
module Label = Name.Make ()

type ty =
  | I1                                      (* https://llvm.org/docs/LangRef.html#integer-type *)
  | I32                                     (* https://llvm.org/docs/LangRef.html#integer-type *)
  | Ptr                                     (* https://llvm.org/docs/LangRef.html#pointer-type *)
  | Struct of ty Iarray.t                   (* https://llvm.org/docs/LangRef.html#structure-type *)
  (* ... *)

(** Operands *)
type opr =
  | I1 of bool                              (* https://llvm.org/docs/LangRef.html#simple-constants *)
  | I32 of Int32.t                          (* https://llvm.org/docs/LangRef.html#simple-constants *)
  | Null                                    (* https://llvm.org/docs/LangRef.html#simple-constants *)
  (* ... *)
  | Global of Global_id.t
  | Local of Local_id.t

(** Comparison condition codes *)
type icmp_cond =    (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Eq              (* equal *)
  (* ... *)

(** Instructions that produce values *)
type value_instr =
  (* Binary Operations *)
  | Add of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#add-instruction *)
  | Sub of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#sub-instruction *)
  | Mul of ty * opr * opr                   (* https://llvm.org/docs/LangRef.html#mul-instruction *)
  (* ... *)

  (* Memory Access and Addressing Operations *)
  | Load of ty * opr                        (* https://llvm.org/docs/LangRef.html#load-instruction *)
  | Store of ty * opr * opr                 (* https://llvm.org/docs/LangRef.html#store-instruction *)
  | Getelementptr of ty * opr * (ty * opr) Iarray.t   (* https://llvm.org/docs/LangRef.html#getelementptr-instruction *)
  (* ... *)

  (* Conversion Operations *)
  | Ptrtoint of ty * opr * ty               (* https://llvm.org/docs/LangRef.html#ptrtoint-to-instruction *)
  (* ... *)

  (* Other Operations *)
  | Icmp of icmp_cond * ty * opr * opr      (* https://llvm.org/docs/LangRef.html#icmp-instruction *)
  | Phi of ty * (opr * Label.t) Iarray.t    (* https://llvm.org/docs/LangRef.html#phi-instruction *)
  | Call of ty * opr * (ty * opr) Iarray.t  (* https://llvm.org/docs/LangRef.html#call-instruction *)
  (* ... *)

(** Terminator instructions *)
type term_instr =                           (* Terminator instructions  https://llvm.org/docs/LangRef.html#terminator-instructions *)
  | Br of Label.t                           (* Unconditional branch     https://llvm.org/docs/LangRef.html#i-br *)
  | Br_i1 of opr * Label.t * Label.t        (* Conditional branch       https://llvm.org/docs/LangRef.html#i-br *)
  | Ret of ty * opr                         (* Return instruction       https://llvm.org/docs/LangRef.html#ret-instruction *)
  (* ... *)

(** Basic blocks *)
type block = {
  label : Label.t;
  instrs : (Local_id.t option * value_instr) Iarray.t;
  term : term_instr;
}

(* Control flow graph of a function *)
type cfg = {
  blocks : block Iarray.t;
}

type param = ty * Local_id.t option

(** Linkage types *)
type linkage =                              (* https://llvm.org/docs/LangRef.html#linkage-types *)
  | Private
  (* ... *)

(** Function declarations *)
type fun_decl = {                           (* https://llvm.org/docs/LangRef.html#functions *)
  linkage : linkage option;
  result_ty : ty;
  params : param Iarray.t;
}

(** Function definitions *)
type fun_def = {                            (* https://llvm.org/docs/LangRef.html#functions *)
  linkage : linkage option;
  result_ty : ty;
  params : param Iarray.t;
  cfg : cfg;
}

(** Modules *)
type module_ = {                            (* https://llvm.org/docs/LangRef.html#id2033 *)
  fun_decls : (Global_id.t * fun_decl) Iarray.t;
  fun_defs : (Global_id.t * fun_def) Iarray.t;
}

(** Output the AST in LLVM’s human readable assembly language representation *)
module Output_ll : sig

  val pp_module : module_ -> Format.formatter -> unit
  val pp_block : block -> Format.formatter -> unit
  val pp_param : param -> Format.formatter -> unit
  val pp_ty : ty -> Format.formatter -> unit

  val pp_global_id : Global_id.t -> Format.formatter -> unit
  val pp_local_id : Local_id.t -> Format.formatter -> unit

end = struct

  let pp_comma_sep ppf () = Format.fprintf ppf ",@ "
  let pp_iarray ?pp_sep f list ppf =
    Format.pp_print_iter Iarray.iter (Fun.flip f) ppf list ?pp_sep
  let pp_seq ?pp_sep f list ppf =
    Format.pp_print_seq (Fun.flip f) ppf list ?pp_sep

  let pp_global_id (id : Global_id.t) = Format.dprintf "%s%t" "@" (Global_id.pp id)
  let pp_local_id (id : Local_id.t) = Format.dprintf "%s%t" "%" (Local_id.pp id)
  let pp_label (id : Label.t) = Format.dprintf "%s%t" "%" (Label.pp id)

  let rec pp_ty (ty : ty) =
    match ty with
    | I1 -> Format.dprintf "i1"
    | I32 -> Format.dprintf "i32"
    | Ptr -> Format.dprintf "ptr"
    | Struct tys -> Format.dprintf "@[{%t}@]" (pp_iarray pp_ty tys ~pp_sep:pp_comma_sep)

  let pp_opr (opr : opr) =
    match opr with
    | I1 true -> Format.dprintf "true"
    | I1 false -> Format.dprintf "false"
    | I32 int -> Format.dprintf "%li" int
    | Null -> Format.dprintf "null"
    | Global id -> pp_global_id id
    | Local id -> pp_local_id id

  let pp_value_instr (instr : value_instr) =
    let pp_binop_instr (name, ty, opr1, opr2) =
      Format.dprintf "@[%s@ %t@ %t,@ %t@]" name (pp_ty ty) (pp_opr opr1) (pp_opr opr2)
    and pp_pred (opr, label) = Format.dprintf "[@[%t,@ %t@]]" (pp_opr opr) (pp_label label)
    and pp_arg (ty, opr) = Format.dprintf "@[%t@ %t@]" (pp_ty ty) (pp_opr opr)
    in
    match instr with
    | Add (ty, opr1, opr2) -> pp_binop_instr ("add", ty, opr1, opr2)
    | Sub (ty, opr1, opr2) -> pp_binop_instr ("sub", ty, opr1, opr2)
    | Mul (ty, opr1, opr2) -> pp_binop_instr ("mul", ty, opr1, opr2)
    | Ptrtoint (ty1, opr, ty2) ->
        Format.dprintf "@[ptrtoint@ %t@ %t@ to@ %t@]" (pp_ty ty1) (pp_opr opr) (pp_ty ty2)
    | Load (ty, ptr) ->
        Format.dprintf "@[load@ %t,@ ptr@ %t@]" (pp_ty ty) (pp_opr ptr)
    | Store (ty, value, ptr) ->
        Format.dprintf "@[<2>@[store@ %t@ %t@],@ @[ptr@ %t@]@]"
          (pp_ty ty) (pp_opr value) (pp_opr ptr)
    | Getelementptr (ty, ptr, elems) ->
        let pp_elem (ty, idx) = Format.dprintf "%t@ %t" (pp_ty ty) (pp_opr idx) in
        Format.dprintf "@[<hv 2>@[getelementptr@ %t,@ @[ptr@ %t@]@],@ @[%t@]@]"
          (pp_ty ty)
          (pp_opr ptr)
          (elems |> pp_iarray pp_elem ~pp_sep:pp_comma_sep)
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

  let pp_term_instr (term : term_instr) =
    match term with
    | Br dest -> Format.dprintf "@[  @[br@ label@ %t@]@]" (pp_label dest)
    | Br_i1 (cond, if_true, if_false) ->
        Format.dprintf "@[  @[br@ i1@ %t,@ label@ %t,@ label@ %t@]@]"
          (pp_opr cond) (pp_label if_true) (pp_label if_false)
    | Ret (ty, opr) -> Format.dprintf "@[  @[ret@ %t@ %t@]@]" (pp_ty ty) (pp_opr opr)

  let pp_instr (id, instr : Local_id.t option * value_instr) =
    match id, instr with
    | Some id, instr ->
        Format.dprintf "@[  @[<2>@[%t@ =@]@ %t@]@]" (pp_local_id id) (pp_value_instr instr)
    | None, instr ->
        Format.dprintf "@[  %t@]" (pp_value_instr instr)

  let rec pp_block ({ label; instrs; term } : block) =
    if Iarray.length instrs = 0 then
      Format.dprintf "%t:@,%t"
        (Label.pp label)
        (pp_term_instr term)
    else
      Format.dprintf "%t:@,%t@,%t"
        (Label.pp label)
        (instrs |> pp_iarray pp_instr)
        (pp_term_instr term)

  let pp_linkage (linkage : linkage) =
    match linkage with
    | Private -> Format.dprintf "private"

  let pp_param (ty, id : param) =
    match id with
    | None -> pp_ty ty
    | Some id -> Format.dprintf "@[%t@ %t@]" (pp_ty ty) (pp_local_id id)

  let pp_fun_decl (id, { linkage; result_ty; params } : Global_id.t * fun_decl) =
    Format.dprintf "@[<v>@[declare@ %t%t@ %t(%t)@."
      (match linkage with
        | None -> Format.dprintf ""
        | Some linkage -> Format.dprintf "%t@ " (pp_linkage linkage))
      (pp_ty result_ty)
      (pp_global_id id)
      (pp_iarray pp_param params ~pp_sep:pp_comma_sep)

  let pp_fun_def (id, { linkage; result_ty; params; cfg } : Global_id.t * fun_def) =
    Format.dprintf "@[<v>@[define@ %t%t@ %t(%t)@ {@]@ %t@ }@]@."
      (match linkage with
        | None -> Format.dprintf ""
        | Some linkage -> Format.dprintf "%t@ " (pp_linkage linkage))
      (pp_ty result_ty)
      (pp_global_id id)
      (pp_iarray pp_param params ~pp_sep:pp_comma_sep)
      (cfg.blocks |> pp_iarray pp_block)

  let pp_module ({ fun_decls; fun_defs } : module_) =
    pp_seq ( @@ ) (Seq.concat @@ Iarray.to_seq [|
      Iarray.to_seq fun_decls |> Seq.map pp_fun_decl;
      Iarray.to_seq fun_defs |> Seq.map pp_fun_def;
    |])

end

(** Output the AST in Graphviz’s {{: https://graphviz.org/doc/info/lang.html}
    DOT language}. This can help with visualising control flow graphs. *)
module Output_dot = struct

  let pp_block (fun_id : Global_id.t) (block : block) (out : Out_channel.t) = begin
    let rec outgoing_labels block =
      match block.term with
      | Br label -> [label]
      | Br_i1 (_, label1, label2) -> [label1; label2]
      | Ret (_, _) -> []
    in

    let label_string label =
      Format.asprintf "\"%t.%t\"" (Global_id.pp fun_id) (Label.pp label)
    in

    let block_text =
      Format.asprintf "@[<v>%t@]" (Output_ll.pp_block block)
    in

    (* Basic block *)
    Printf.fprintf out "\n";
    Printf.fprintf out "    %s [\n" (label_string block.label);
    Printf.fprintf out "      label=<<table color=\"black\" border=\"0\" cellborder=\"0\" cellpadding=\"3\">\n";
    Printf.fprintf out "        <th><td align=\"left\" border=\"1\" sides=\"b\">%s:</td></th>\n" (Label.to_string block.label);
    String.split_on_char '\n' block_text |> List.iter (Printf.fprintf out "        <tr><td align=\"left\">%s</td></tr>\n");
    Printf.fprintf out "      </table>>\n";
    Printf.fprintf out "    ];\n";

    (* Control flow edges *)
    outgoing_labels block |> List.iter begin fun end_label ->
      Printf.fprintf out "    %s -> %s;\n" (label_string block.label) (label_string end_label)
    end;
  end

  let pp_module ({ fun_defs; _ } : module_) (out : Out_channel.t) = begin
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
    fun_defs |> Iarray.iter begin fun (id, { result_ty; params; cfg }) ->
      Printf.fprintf out "  subgraph \"%s\" {\n" (Global_id.to_string id);

      (* Function signature *)
      Printf.fprintf out "    label=<<table color=\"black\" border=\"0\" cellborder=\"0\" cellpadding=\"3\">\n";
      Printf.fprintf out "      <th><td align=\"left\" border=\"1\" sides=\"b\">\n";
      Printf.fprintf out "        %s %s(%t)\n"
        (Output_ll.pp_ty result_ty |> Format.asprintf "%t")
        (Output_ll.pp_global_id id |> Format.asprintf "%t")
        (fun out ->
          params |> Iarray.iteri begin fun i param ->
            if i <> 0 then Printf.fprintf out ", ";
            Printf.fprintf out "%s" (Format.asprintf "%t" (Output_ll.pp_param param));
          end);
      Printf.fprintf out "      </td></th>\n";
      Printf.fprintf out "    </table>>;\n";
      Printf.fprintf out "    cluster=true;\n";

      (* Control flow graph *)
      cfg.blocks |> Iarray.iter begin fun block ->
        pp_block id block out;
      end;

      Printf.fprintf out "  }\n";
    end;

    Printf.fprintf out "}\n";
  end

end
