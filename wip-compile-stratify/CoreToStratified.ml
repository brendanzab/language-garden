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
      begin match translate env def with
      | Tm2 _ -> failwith "bug: term is too large for definition"
      | Tm1 def ->
          begin match translate (bind_entry1 env) body with
          | Tm2 _ -> failwith "bug: term is too large for body of let expression"
          | Tm1 body -> Tm1 (Let11 (name, def, body))
          | Tm0 body -> Tm0 (Let10 (name, def, body))
          end
      | Tm0 def ->
          begin match translate (bind_entry0 env) body with
          | Tm2 _ -> failwith "bug: term is too large for body of let expression"
          | Tm1 body -> Tm1 (Let01 (name, def, body))
          | Tm0 body -> Tm0 (Let00 (name, def, body))
          end
      end
  | Ann (expr, ty) ->
      begin match translate env expr, translate env ty with
      | Tm1 expr, Tm2 ty -> Tm1 (Ann1 (expr, ty))
      | Tm0 expr, Tm1 ty -> Tm0 (Ann0 (expr, ty))
      | _ -> failwith "bug: mismatched annotation universe"
      end
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 level -> Tm1 (Var1 (Env.level_to_index env.size1 level))
      | Level0 level -> Tm0 (Var0 (Env.level_to_index env.size0 level))
      end
  | Univ L0 -> Tm2 Univ0
  | Univ L1 -> failwith "bug: universe exceeds size of target language"
  | FunType (name, param_ty, body_ty) ->
      begin match translate env param_ty with
      | Tm2 param_ty ->
          begin match translate (bind_entry1 env) body_ty with
          | Tm2 body_ty -> Tm2 (FunType11 (name, param_ty, body_ty))
          | Tm1 _ -> failwith "bug: level 1 parameter type found in a level 0 function type"
          | Tm0 _ -> failwith "bug: level 0 terms are too small to contain types"
          end
      | Tm1 param_ty ->
          begin match translate (bind_entry0 env) body_ty with
          | Tm2 body_ty -> Tm2 (FunType01 (name, param_ty, body_ty))
          | Tm1 body_ty -> Tm1 (FunType00 (name, param_ty, body_ty))
          | Tm0 _ -> failwith "bug: level 0 terms are too small to contain types"
          end
      | Tm0 _ -> failwith "bug: level 0 terms are too small to contain types"
      end
  | FunLit (name, param_ty, body) ->
      begin match translate env param_ty with
      | Tm2 param_ty ->
          begin match translate (bind_entry1 env) body with
          | Tm2 _ -> failwith "bug: body of function literal is too large"
          | Tm1 body -> Tm1 (FunLit11 (name, param_ty, body))
          | Tm0 _ -> failwith "bug: level 1 parameter type found in a level 0 function literal"
          end
      | Tm1 param_ty ->
          begin match translate (bind_entry0 env) body with
          | Tm2 _ -> failwith "bug: body of function literal is too large"
          | Tm1 body -> Tm1 (FunLit01 (name, param_ty, body))
          | Tm0 body -> Tm0 (FunLit00 (name, param_ty, body))
          end
      | Tm0 _ -> failwith "bug: level 0 terms are too small to contain types"
      end
  | FunApp (head, arg) ->
      begin match translate env head, translate env arg with
      | Tm1 head, Tm1 arg -> Tm1 (FunApp11 (head, arg))
      | Tm1 head, Tm0 arg -> Tm1 (FunApp01 (head, arg))
      | Tm0 head, Tm0 arg -> Tm0 (FunApp00 (head, arg))
      | _ -> failwith "bug: mismatched argument universe"
      end
