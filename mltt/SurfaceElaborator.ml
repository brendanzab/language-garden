module List = Base.List
module Syntax = SurfaceSyntax

type context = {
  level : Core.Semantics.level;
  names : (string option) list;
  types : Core.Semantics.value list;
  exprs : Core.Semantics.value list;
}

let initial_context = {
  level = 0;
  names = [];
  types = [];
  exprs = [];
}

let bind_def context name ty expr = {
  level = context.level + 1;
  names = name :: context.names;
  types = ty :: context.types;
  exprs = expr :: context.exprs;
}

let bind_param context name ty = {
  level = context.level + 1;
  names = name :: context.names;
  types = ty :: context.types;
  exprs = Core.Semantics.Neutral (Core.Semantics.Var context.level) :: context.exprs;
}

let lookup context name =
  let (let*) = Option.bind in
  let* index = List.elem_index (Some name) context.names in
  let* ty = List.nth_opt context.types index in
  Some (index, ty)

let norm context =
  Core.Semantics.norm context.exprs

let eval context =
  Core.Semantics.eval context.exprs
let quote context =
  Core.Semantics.quote context.level

let is_convertible context =
  Core.Semantics.is_convertible (List.length context.exprs)

let (let*) = Result.bind
(* let (let+) f x = Result.map x f *)

let rec check_ty context ty =
  check_term context ty Core.Semantics.UnivType

and check_term context expr expected_ty =
  match expr, expected_ty with
  | Syntax.Let (def_name, def_ty, def_expr, body_expr), _ ->
      let* def_ty = check_ty context def_ty in
      let def_ty' = eval context def_ty in
      let* def_expr = check_term context def_expr def_ty' in
      let body_context = bind_def context def_name def_ty' (eval context def_expr) in
      let* body_expr = check_term body_context body_expr expected_ty in
      Ok (Core.Syntax.Let (def_ty, def_expr, body_expr))
  | Syntax.FunctionLit ((param_name, None), body_expr), Core.Semantics.TypeFunction (param_ty, body_ty) ->
      let param_expr = Core.Semantics.Neutral (Core.Semantics.Var context.level) in
      let body_ty = Core.Semantics.closure_app body_ty param_expr in
      let body_context = bind_param context param_name param_ty in
      let* body_expr = check_term body_context body_expr body_ty in
      Ok (Core.Syntax.FunctionLit (quote context param_ty, body_expr))
  | Syntax.RecordLit fields, Core.Semantics.TypeRecord (labels, tys) ->
      let rec check_fields fields labels tys =
        match fields, labels, Core.Semantics.telescope_uncons tys with
        | [], [], None -> Ok []
        | [], _, _ -> Error "not enough fields in record literal"
        | _, [], None -> Error "too many fields in record literal"
        | _, [], Some _ | _, _::_, None -> Error "bug: mismatched labels and telescope"
        | (label, _) :: _, label' :: _, _ when label <> label' ->
            Error ("unexpected field, found `" ^ label ^ "` expected `" ^ label' ^ "`")
        | (label, expr) :: fields, _ :: labels, Some (ty, tys) ->
            let* expr = check_term context expr ty in
            let* fields = check_fields fields labels (tys (eval context expr)) in
            Ok ((label, expr) :: fields)
      in
      let* fields = check_fields fields labels tys in
      Ok (Core.Syntax.RecordLit fields)
  | Syntax.Unit, Core.Semantics.UnivType ->
      Ok (Core.Syntax.TypeRecord [])
  | Syntax.Unit, Core.Semantics.TypeRecord ([], _) ->
      Ok (Core.Syntax.RecordLit [])
  | expr, _ ->
      let* (expr, ty) = synth_term context expr in
      if is_convertible context ty expected_ty then
        Ok expr
      else
        Error "mismatched types"

and synth_term context expr =
  match expr with
  | Syntax.Name name -> begin
    match lookup context name with
    | Some (index, ty) -> Ok (Core.Syntax.Var index, ty)
    | None -> Error ("name `" ^ name ^ "` not in scope")
  end
  | Syntax.Ann (expr, ty) ->
      let* ty = check_ty context ty in
      let ty' = eval context ty in
      let* expr = check_term context expr ty' in
      Ok (expr, ty')
  | Syntax.Let (def_name, def_ty, def_expr, body_expr) ->
      let* def_ty = check_ty context def_ty in
      let def_ty' = eval context def_ty in
      let* def_expr = check_term context def_expr def_ty' in
      let body_context = bind_def context def_name def_ty' (eval context def_expr) in
      let* (body_expr, body_ty) = synth_term body_context body_expr in
      Ok (Core.Syntax.Let (def_ty, def_expr, body_expr), body_ty)
  | Syntax.Type ->
      Ok (Core.Syntax.UnivType, Core.Semantics.UnivType)
  | Syntax.Arrow (param_ty, body_ty) ->
      let* param_ty = check_ty context param_ty in
      let body_context = bind_param context None (eval context param_ty) in
      let* body_ty = check_ty body_context body_ty in
      Ok (Core.Syntax.TypeFunction (param_ty, body_ty), Core.Semantics.UnivType)
  | Syntax.FunctionType ((param_name, Some param_ty), body_ty) ->
      let* param_ty = check_ty context param_ty in
      let body_context = bind_param context param_name (eval context param_ty) in
      let* body_ty = check_ty body_context body_ty in
      Ok (Core.Syntax.TypeFunction (param_ty, body_ty), Core.Semantics.UnivType)
  | Syntax.FunctionType ((_, None), _) ->
      Error "type annotations required for function type parameter"
  | Syntax.FunctionLit (_, _) ->
      Error "type annotations required for function literal"
  | Syntax.RecordType fields ->
      let rec check_fields context seen_labels = function
        | [] -> Ok []
        | (label, _) :: _ when List.mem label seen_labels ->
            Error ("duplicate label `" ^ label ^ "` encountered in record type")
        | (label, ty) :: fields ->
            let* ty = check_ty context ty in
            let fields_context = bind_param context (Some label) (eval context ty) in
            let* fields = check_fields fields_context (label :: seen_labels) fields in
            Ok ((label, ty) :: fields)
      in
      let* fields = check_fields context [] fields in
      Ok (Core.Syntax.TypeRecord fields, Core.Semantics.UnivType)
  | Syntax.RecordLit _ ->
      Error "type annotations required for record literal"
  | Syntax.Unit ->
      Error "type annotations required for unit literal"
  | Syntax.App (head_expr, arg_exprs) ->
      List.fold_left
        (fun head_expr arg_expr ->
          let* (head_expr, head_ty) = head_expr in
          match head_ty with
          | Core.Semantics.TypeFunction (param_ty, body_ty) ->
              let* arg_expr = check_term context arg_expr param_ty in
              let body_ty = Core.Semantics.closure_app body_ty (eval context arg_expr) in
              Ok (Core.Syntax.FunctionApp (head_expr, arg_expr), body_ty)
          | _ -> Error "function expected")
        (synth_term context head_expr)
        arg_exprs
  | Syntax.Proj (head_expr, label) ->
      let* (head_expr, head_ty) = synth_term context head_expr in
      match head_ty with
      | Core.Semantics.TypeRecord (labels, tys) ->
          let head_expr' = eval context head_expr in
          let rec get_ty labels tys =
            match labels, Core.Semantics.telescope_uncons tys with
            | [], None -> Error ("label `" ^ label ^ "` not found in record")
            | [], _ | _::_, None -> Error ("bug: mismatched labels and telescope")
            | label' :: _, Some (ty, _) when label = label' -> Ok ty
            | label' :: labels, Some (_, tys) ->
                get_ty labels (tys (Core.Semantics.record_proj head_expr' label'))
          in
          let* ty = get_ty labels tys in
          Ok (Core.Syntax.RecordProj (head_expr, label), ty)
      | _ -> Error "record expected"
