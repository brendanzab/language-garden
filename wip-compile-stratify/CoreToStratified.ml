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
  names : (Core.Ns.tm, Core.name) Env.t;
  levels : (Core.Ns.tm, level) Env.t;
}

let empty_env = {
  size1 = Env.empty_size;
  size0 = Env.empty_size;
  names = Env.empty;
  levels = Env.empty;
}

(** Add a level 1 binding to the environment *)
let bind_entry1 name env = {
  env with
  size1 = env.size1 |> Env.bind_level;
  names = env.names |> Env.bind_entry name;
  levels = env.levels |> Env.bind_entry (Level1 (Env.next_level env.size1));
}

(** Add a level 0 binding to the environment *)
let bind_entry0 name env = {
  env with
  size0 = env.size0 |> Env.bind_level;
  names = env.names |> Env.bind_entry name;
  levels = env.levels |> Env.bind_entry (Level0 (Env.next_level env.size0));
}


(** {1 Translation} *)

let rec translate env : Core.Syntax.tm -> tm =
  function
  | Let (name, def, body) ->
      let def = translate_tm env def in
      let body =
        match def with
        | Tm1 _ -> translate_tm (bind_entry1 name env) body
        | Tm0 _ -> translate_tm (bind_entry0 name env) body
      in
      begin match body with
      | Tm1 body -> Tm1 (Let1 (name, def, body))
      | Tm0 body -> Tm0 (Let0 (name, def, body))
      end
  | Ann (expr, ty) ->
      begin match translate_ty env ty with
      | Ty1 ty -> Tm1 (Ann1 (translate1 env expr, ty))
      | Ty0 ty -> Tm0 (Ann0 (translate0 env expr, ty))
      end
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 level -> Tm1 (Var1 (Env.level_to_index env.size1 level))
      | Level0 level -> Tm0 (Var0 (Env.level_to_index env.size0 level))
      end
  | Univ -> Tm2 Univ
  | FunType (name, param_ty, body_ty) ->
      let param_ty = translate_ty env param_ty in
      let body_ty =
        match param_ty with
        | Ty1 _ -> translate_ty (bind_entry1 name env) body_ty
        | Ty0 _ -> translate_ty (bind_entry0 name env) body_ty
      in
      begin match param_ty, body_ty with
      | param_ty, Ty1 body_ty -> Tm2 (FunType1 (name, param_ty, body_ty))
      | Ty0 param_ty, Ty0 body_ty -> Tm1 (FunType0 (name, param_ty, body_ty))
      | Ty1 _, Ty0 _ -> failwith "level 1 parameter type found in a level 0 function type"
      end
  | FunLit (name, param_ty, body) ->
      let param_ty = translate_ty env param_ty in
      let body =
        match param_ty with
        | Ty1 _ -> translate_tm (bind_entry1 name env) body
        | Ty0 _ -> translate_tm (bind_entry0 name env) body
      in
      begin match param_ty, body with
      | param_ty, Tm1 body -> Tm1 (FunLit1 (name, param_ty, body))
      | Ty0 param_ty, Tm0 body -> Tm0 (FunLit0 (name, param_ty, body))
      | Ty1 _, Tm0 _ -> failwith "level 1 parameter type found in a level 0 function literal"
      end
  | FunApp (head, arg) ->
      begin match translate_tm env head with
      | Tm1 head -> Tm1 (FunApp1 (head, translate_tm env arg))
      | Tm0 head -> Tm0 (FunApp0 (head, translate0 env arg))
      end

and translate1 env (tm : Core.Syntax.tm) : Stratified.Syntax.tm1 =
  match translate env tm with
  | Tm1 tm -> tm
  | Tm2 _ | Tm0 _ -> failwith "expected a level 1 term"

and translate0 env (tm : Core.Syntax.tm) : Stratified.Syntax.tm0 =
  match translate env tm with
  | Tm0 tm -> tm
  | Tm2 _ | Tm1 _ -> failwith "expected a level 0 term"

and translate_ty env (tm : Core.Syntax.tm) : Stratified.Syntax.ty =
  match translate env tm with
  | Tm2 ty -> Ty1 ty
  | Tm1 ty -> Ty0 ty
  | _ -> failwith "level 0 terms should not contain types"

and translate_tm env (tm : Core.Syntax.tm) : Stratified.Syntax.tm =
  match translate env tm with
  | Tm2 _ -> failwith "expected a level 0 or 1 term"
  | Tm1 tm -> Tm1 tm
  | Tm0 tm -> Tm0 tm
