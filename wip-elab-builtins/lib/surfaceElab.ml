type context = {
  item_vtys : (string * Core.Semantics.value) list;
  item_vtms : (string * Core.Semantics.value) list;
}

let initial_context = {
  item_vtys = [];
  item_vtms = [];
}

let add_item context ~label ~vty ~vtm = {
  item_vtys = (label, vty) :: context.item_vtys;
  item_vtms = (label, vtm) :: context.item_vtms;
}

let eval context =
  Core.Semantics.eval (fun name -> List.assoc name context.item_vtms)
let quote = Core.Semantics.quote
let is_convertible = Core.Semantics.is_convertible

let lookup_item_vty context name =
  List.assoc_opt name context.item_vtys

let rec check_tm context (tm : SurfaceSyntax.tm) (vty : Core.Semantics.value) : Core.Syntax.tm =
  match tm, vty with
  | NumLit num, I32Type -> I32Lit (Int32.of_string num)
  | NumLit num, I64Type -> I64Lit (Int64.of_string num)
  | NumLit num, F64Type -> F64Lit (Float.of_string num)
  | tm, vty ->
      let tm, vty' = synth_tm context tm in
      if is_convertible vty vty' then tm else
        failwith (Format.asprintf "type mismatch: expected `%a`, found `%a`"
          Core.Syntax.pp_tm (Core.Semantics.quote vty)
          Core.Syntax.pp_tm (Core.Semantics.quote vty'))

and synth_tm context : SurfaceSyntax.tm -> Core.Syntax.tm * Core.Semantics.value =
  function
  | Path ["builtin"; "Type"] -> Type, Type
  | Path ["builtin"; "Bool"] -> BoolType, Type
  | Path ["builtin"; "true"] -> BoolLit true, BoolType
  | Path ["builtin"; "false"] -> BoolLit false, BoolType
  | Path ["builtin"; "I32"] -> I32Type, Type
  | Path ["builtin"; "I64"] -> I64Type, Type
  | Path ["builtin"; "F64"] -> F64Type, Type
  | Path [name] ->
      begin match lookup_item_vty context name with
      | Some ty -> ItemVar name, ty
      | None -> failwith ("unbound path: " ^ name)
      end
  | Path path ->
      failwith ("unbound path: " ^ String.concat "." path)
  | NumLit _num ->
      (* TODO: postponed elaboration? *)
      failwith "ambiguous numeric literal"
  | Binop (lhs, _op, rhs) ->
      (* TODO: unification? *)
      begin match synth_tm context lhs with
      | _lhs, (I32Type | I64Type | F64Type as vty) ->
          let _rhs = check_tm context rhs vty in
          failwith "todo"
      | _ -> failwith "unexpected type in lhs"
      end

let elab_item context : SurfaceSyntax.item -> Core.Syntax.item * context  =
  function
  | Use _ -> failwith "todo"
  | Def def ->
      begin match def.ty with
      | Some ty ->
          let ty = check_tm context ty Type in
          let vty = eval context ty in
          let tm = check_tm context def.tm vty in
          let vtm = eval context tm in
          let context = add_item context ~label:def.label ~vty ~vtm in
          Def { label = def.label; ty; tm }, context
      | None ->
          let tm, vty = synth_tm context def.tm in
          let ty = quote vty in
          let vtm = eval context tm in
          let context = add_item context ~label:def.label ~vty ~vtm in
          Def { label = def.label; ty; tm }, context
      end

let [@tail_mod_cons] rec elab_items context =
  function
  | [] -> []
  | item :: items ->
      let item, context = elab_item context item in
      item :: elab_items context items
