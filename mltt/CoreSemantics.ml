module Syntax = CoreSyntax

type level = int

type value =
  | Neutral of neu
  | UnivType
  | TypeFunction of value * closure
  | TypeRecord of string list * telescope
  | FunctionLit of value * closure
  | RecordLit of (string * value) list
and neu =
  | Var of level
  | FunctionApp of neu * value
  | RecordProj of neu * string
and closure = {
  env : env;
  term : Syntax.term;
}
and telescope =
  | Nil
  | Cons of value * env * Syntax.term list
and env = value list

exception Error of string

let rec eval env : Syntax.term -> value = function
  | Syntax.Var index -> begin
    match List.nth_opt env index with
    | Some value -> value
    | None -> raise (Error ("var not found @" ^ string_of_int index))
  end
  | Syntax.Let (_, def_expr, body_expr) ->
      eval (eval env def_expr :: env) body_expr
  | Syntax.UnivType -> UnivType
  | Syntax.TypeFunction (param_ty, body_ty) ->
      TypeFunction (eval env param_ty, { env; term = body_ty })
  | Syntax.TypeRecord [] -> TypeRecord ([], Nil)
  | Syntax.TypeRecord ((label, ty) :: fields) ->
      let labels, tys = List.split fields in
      TypeRecord (label :: labels, Cons (eval env ty, env, tys))
  | Syntax.FunctionLit (param_ty, body) ->
      FunctionLit (eval env param_ty, { env; term = body })
  | Syntax.FunctionApp (head, arg) ->
      function_app (eval env head) (eval env arg)
  | Syntax.RecordLit fields ->
      let eval_field (label, expr) = (label, eval env expr) in
      RecordLit (List.map eval_field fields)
  | Syntax.RecordProj (expr, label) ->
      record_proj (eval env expr) label
and closure_app closure arg =
  eval (arg :: closure.env) closure.term
and function_app expr arg =
  match expr with
  | Neutral neu -> Neutral (FunctionApp (neu, arg))
  | FunctionLit (_, body) -> closure_app body arg
  | _ -> raise (Error "invalid record projection")
and record_proj expr label =
  match expr with
  | Neutral neu -> Neutral (RecordProj (neu, label))
  | RecordLit fields -> begin
      match fields |> List.find_opt (fun (l, _) -> l = label) with
      | Some (_, expr) -> expr
      | None -> raise (Error "field not found")
  end
  | _ -> raise (Error "invalid record projection")

let telescope_uncons = function
  | Nil -> None
  | Cons (value, _, []) ->
      Some (value, fun _ -> Nil)
  | Cons (value, env, term :: terms) ->
      let values value =
        let env = value :: env in
        Cons (eval env term, env, terms) in
      Some (value, values)

let rec quote size : value -> Syntax.term = function
  | Neutral neu -> quote_neu size neu
  | UnivType -> UnivType
  | TypeFunction (param_ty, body_ty) ->
      TypeFunction (quote size param_ty, quote_closure size body_ty)
  | TypeRecord (labels, tys) ->
      let rec quote_fields size labels tys =
        match labels, telescope_uncons tys with
        | [], None -> []
        | [], _ | _, None -> raise (Error "invalid record type telescope")
        | label :: labels, Some (ty, tys) ->
            let var = Neutral (Var size) in
            (label, quote size ty) :: quote_fields (size + 1) labels (tys var)
      in
      TypeRecord (quote_fields size labels tys)
  | FunctionLit (param_ty, body_expr) ->
      FunctionLit (quote size param_ty, quote_closure size body_expr)
  | RecordLit fields ->
      RecordLit (List.map (fun (label, expr) -> label, quote size expr) fields)
and quote_neu size : neu -> Syntax.term = function
  | Var level -> Var (size - level - 1)
  | FunctionApp (neu, arg) -> FunctionApp (quote_neu size neu, quote size arg)
  | RecordProj (neu, label) -> RecordProj (quote_neu size neu, label)
and quote_closure size closure =
  let var = Neutral (Var size) in
  quote (size + 1) (closure_app closure var)

let norm env term =
  let value = eval env term
  in quote (List.length env) value

let rec is_convertible size value1 value2 =
  match value1, value2 with
  | Neutral nue1, Neutral nue2 ->
      is_convertible_neu size nue1 nue2
  | UnivType, UnivType -> true
  | TypeFunction (param_ty1, body_ty1), TypeFunction (param_ty2, body_ty2) ->
      is_convertible size param_ty1 param_ty2
        && is_convertible_closure size body_ty1 body_ty2
  | TypeRecord (labels1, tys1), TypeRecord (labels2, tys2) ->
      labels1 = labels2
        && is_convertible_tele size tys1 tys2
  | FunctionLit (param_ty1, body_expr1), FunctionLit (param_ty2, body_expr2) ->
      is_convertible size param_ty1 param_ty2
        && is_convertible_closure size body_expr1 body_expr2
  | RecordLit fields1, RecordLit fields2 ->
      let on_field (label1, expr1) (label2, expr2) =
        label1 = label2 && is_convertible size expr1 expr2 in
      List.for_all2 on_field fields1 fields2
  | _, _ -> false
and is_convertible_neu size nue1 nue2 =
  match nue1, nue2 with
  | Var var1, Var var2 -> var1 = var2
  | FunctionApp (neu1, arg1), FunctionApp (neu2, arg2) ->
      is_convertible_neu size neu1 neu2 && is_convertible size arg1 arg2
  | RecordProj (neu1, label1), RecordProj (neu2, label2) ->
      is_convertible_neu size neu1 neu2 && label1 = label2
  | _, _ -> false
and is_convertible_closure size closure1 closure2 =
  let var = Neutral (Var size) in
  is_convertible (size + 1) (closure_app closure1 var) (closure_app closure2 var)
and is_convertible_tele size tele1 tele2 =
  match telescope_uncons tele1, telescope_uncons tele2 with
  | None, None -> true
  | Some _, None | None, Some _ -> false
  | Some (value1, tele1), Some (value2, tele2) ->
      let var = Neutral (Var size) in
      is_convertible size value1 value2
        && is_convertible_tele (size + 1) (tele1 var) (tele2 var)
