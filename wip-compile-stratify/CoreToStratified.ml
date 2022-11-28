type tm =
  | Tm2 of Stratified.Syntax.tm2
  | Tm1 of Stratified.Syntax.tm1
  | Tm0 of Stratified.Syntax.tm0


type level =
  | Level1 of Stratified.Ns.tm1 Env.level
  | Level0 of Stratified.Ns.tm0 Env.level


(** {1 Compilation Environment} *)

(** The stratified syntax has two namespaces: one for level 1 terms, and another
    for level 0 terms. The compilation environment keeps track of the number of
    entries bound for each namespace, with a mapping from bindings in the core
    language bindings in the stratified language. *)

type env = {
  (* Current size of the stratified environments *)
  size1 : Stratified.Ns.tm1 Env.size;
  size0 : Stratified.Ns.tm0 Env.size;
  (* Mappings from the core language bindings to stratified bindings *)
  levels : (Core.Ns.tm, level) Env.t;
}

let empty_env = {
  size1 = Env.empty_size;
  size0 = Env.empty_size;
  levels = Env.empty;
}

(** Add a level 1 binding to the environment *)
let bind_entry1 env = {
  env with
  size1 = env.size1 |> Env.bind_level;
  levels = env.levels |> Env.bind_entry (Level1 (Env.next_level env.size1));
}

(** Add a level 0 binding to the environment *)
let bind_entry0 env = {
  env with
  size0 = env.size0 |> Env.bind_level;
  levels = env.levels |> Env.bind_entry (Level0 (Env.next_level env.size0));
}


(** {1 Translation} *)

let rec translate env : Core.Syntax.tm -> tm =
  function
  | Let (name, def, body) ->
      let def, body =
        match translate_tm env def with
        | Tm1 _ as def -> def, translate_tm (bind_entry1 env) body
        | Tm0 _ as def -> def, translate_tm (bind_entry0 env) body
      in
      begin match body with
      | Tm1 body -> Tm1 (Let1 (name, def, body))
      | Tm0 body -> Tm0 (Let0 (name, def, body))
      end
  | Ann (expr, ty) ->
      begin match translate_tm env expr, translate_ty env ty with
      | Tm1 expr, Ty1 ty -> Tm1 (Ann1 (expr, ty))
      | Tm0 expr, Ty0 ty -> Tm0 (Ann0 (expr, ty))
      | Tm1 _, Ty0 _ -> failwith "bug: annotating a level 1 term with a level 0 type"
      | Tm0 _, Ty1 _ -> failwith "bug: annotating a level 0 term with a level 1 type"
      end
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 level -> Tm1 (Var1 (Env.level_to_index env.size1 level))
      | Level0 level -> Tm0 (Var0 (Env.level_to_index env.size0 level))
      end
  | Univ -> Tm2 Univ
  | FunType (name, param_ty, body_ty) ->
      begin match translate_ty env param_ty with
      | Ty1 param_ty ->
          begin match translate_ty (bind_entry1 env) body_ty with
          | Ty1 body_ty -> Tm2 (FunType1 (name, Ty1 param_ty, body_ty))
          | Ty0 _ -> failwith "bug: level 1 parameter type found in a level 0 function type"
          end
      | Ty0 param_ty ->
          begin match translate_ty (bind_entry0 env) body_ty with
          | Ty1 body_ty -> Tm2 (FunType1 (name, Ty0 param_ty, body_ty))
          | Ty0 body_ty -> Tm1 (FunType0 (name, param_ty, body_ty))
          end
      end
  | FunLit (name, param_ty, body) ->
      begin match translate_ty env param_ty with
      | Ty1 param_ty ->
          begin match translate_tm (bind_entry1 env) body with
          | Tm1 body -> Tm1 (FunLit1 (name, Ty1 param_ty, body))
          | Tm0 _ -> failwith "bug: level 1 parameter type found in a level 0 function literal"
          end
      | Ty0 param_ty ->
          begin match translate_tm (bind_entry0 env) body with
          | Tm1 body -> Tm1 (FunLit1 (name, Ty0 param_ty, body))
          | Tm0 body -> Tm0 (FunLit0 (name, param_ty, body))
          end
      end
  | FunApp (head, arg) ->
      begin match translate_tm env head, translate_tm env arg with
      | Tm1 head, arg -> Tm1 (FunApp1 (head, arg))
      | Tm0 head, Tm0 arg -> Tm0 (FunApp0 (head, arg))
      | Tm0 _, Tm1 _ -> failwith "bug: level 1 argument applied to level 0 term"
      end

and translate_ty env (tm : Core.Syntax.tm) : Stratified.Syntax.ty =
  match translate env tm with
  | Tm2 ty -> Ty1 ty
  | Tm1 ty -> Ty0 ty
  | _ -> failwith "bug: level 0 terms should not contain types"

and translate_tm env (tm : Core.Syntax.tm) : Stratified.Syntax.tm =
  match translate env tm with
  | Tm2 _ -> failwith "bug: expected a level 0 or 1 term"
  | Tm1 tm -> Tm1 tm
  | Tm0 tm -> Tm0 tm
