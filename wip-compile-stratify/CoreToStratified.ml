(** {1 Translation environment} *)

type level =
  | Level1 of Stratified.Ns.tm1 Env.level
  | Level0 of Stratified.Ns.tm0 Env.level

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

type tm = [
  | `Tm2 of Stratified.Syntax.tm2
  | `Tm1 of Stratified.Syntax.tm1
  | `Tm0 of Stratified.Syntax.tm0
]

let rec translate env : Core.Syntax.tm -> tm =
  function
  | Let (name, def, body) ->
      begin match translate_tm env def with
      | `Tm1 def ->
          begin match translate_tm (bind_entry1 env) body with
          | `Tm1 body -> `Tm1 (Let11 (name, def, body))
          | `Tm0 body -> `Tm0 (Let10 (name, def, body))
          end
      | `Tm0 def ->
          begin match translate_tm (bind_entry0 env) body with
          | `Tm1 body -> `Tm1 (Let01 (name, def, body))
          | `Tm0 body -> `Tm0 (Let00 (name, def, body))
          end
      end
  | Ann (expr, ty) ->
      begin match translate_ty env ty with
      | `Tm2 ty -> `Tm1 (Ann1 (translate1 env expr, ty))
      | `Tm1 ty -> `Tm0 (Ann0 (translate0 env expr, ty))
      end
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 level -> `Tm1 (Var1 (Env.level_to_index env.size1 level))
      | Level0 level -> `Tm0 (Var0 (Env.level_to_index env.size0 level))
      end
  | Univ L0 -> `Tm2 Univ0
  | Univ L1 -> failwith "bug: universe exceeds size of target language"
  | FunType (name, param_ty, body_ty) ->
      begin match translate_ty env param_ty with
      | `Tm2 param_ty ->
          begin match translate_ty (bind_entry1 env) body_ty with
          | `Tm2 body_ty -> `Tm2 (FunType11 (name, param_ty, body_ty))
          | `Tm1 body_ty -> `Tm2 (FunType10 (name, param_ty, body_ty))
          end
      | `Tm1 param_ty ->
          begin match translate_ty (bind_entry0 env) body_ty with
          | `Tm2 body_ty -> `Tm2 (FunType01 (name, param_ty, body_ty))
          | `Tm1 body_ty -> `Tm1 (FunType00 (name, param_ty, body_ty))
          end
      end
  | FunLit (name, param_ty, body) ->
      begin match translate_ty env param_ty with
      | `Tm2 param_ty ->
          begin match translate_tm (bind_entry1 env) body with
          | `Tm1 body -> `Tm1 (FunLit11 (name, param_ty, body))
          | `Tm0 body -> `Tm1 (FunLit10 (name, param_ty, body))
          end
      | `Tm1 param_ty ->
          begin match translate_tm (bind_entry0 env) body with
          | `Tm1 body -> `Tm1 (FunLit01 (name, param_ty, body))
          | `Tm0 body -> `Tm0 (FunLit00 (name, param_ty, body))
          end
      end
  | FunApp (head, arg) ->
      begin match translate_tm env head with
      | `Tm1 _ ->
          (*  This could either be a [FunApp11], [FunApp01], or [FunApp10], but
              we can’t tell from the term alone. See [translate1] and
              [translate0] for cases where this is not ambiguous.

              I’m not sure if this handles all of the terms that pass validation
              in the [Core.Validation] functions. Perhaps a type-directed
              translation could help make this feel less ad-hoc? Not sure.
          *)
          failwith "bug: ambiguous function application"
      | `Tm0 head -> `Tm0 (FunApp00 (head, translate0 env arg))
      end


(** {1 Translation functions specialised to ranges of term levels} *)

(** The following functions allow us to avoid some additional checks in when we
    know a bit more about the term we’re expecting. *)

(** Translate a term that can be given a type (i.e. that can appear on the left
    of the typing operator). *)
and translate_tm env tm =
  match translate (bind_entry0 env) tm with
  | `Tm1  _ | `Tm0 _ as tm -> tm
  | `Tm2 _ -> failwith "bug: level 2 terms are too large to be typable"

(** Translate a term that can be used as a type in the target language (i.e.
    that can appear on the right of the typing operator). *)
and translate_ty env tm =
  match translate (bind_entry0 env) tm with
  | `Tm2  _ | `Tm1 _ as tm -> tm
  | `Tm0 _ -> failwith "bug: level 0 terms are too small to contain types"


(** {1 Translation functions specialised to known term levels} *)

(** The following translation functions are specialised to specific term levels.
    The idea is to try to supply more information about the expected level of
    the term, in an attempt to avoid ambiguous function applications.
*)

(* FIXME: The following functions feel incredibly ad hoc?? Not sure if there’s
   a nicer way to account for all these cases? *)

and translate2 env : Core.Syntax.tm -> Stratified.Syntax.tm2 =
  function
  | Univ L0 -> Univ0
  | FunType (name, param_ty, body_ty) ->
      begin match translate_ty env param_ty with
      | `Tm2 param_ty ->
          begin match translate_ty (bind_entry1 env) body_ty with
          | `Tm2 body_ty -> FunType11 (name, param_ty, body_ty)
          | `Tm1 body_ty -> FunType10 (name, param_ty, body_ty)
          end
      | `Tm1 param_ty ->
          FunType01 (name, param_ty, translate2 (bind_entry0 env) body_ty)
      end
  | _ -> failwith "bug: expected a level 2 term"

and translate1 env : Core.Syntax.tm -> Stratified.Syntax.tm1 =
  function
  | Let (name, def, body) ->
      begin match translate_tm env def with
      | `Tm1 def -> Let11 (name, def, translate1 (bind_entry1 env) body)
      | `Tm0 def -> Let01 (name, def, translate1 (bind_entry0 env) body)
      end
  | Ann (expr, ty) ->
      Ann1 (translate1 env expr, translate2 env ty)
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 level -> Var1 (Env.level_to_index env.size1 level)
      | Level0 _ -> failwith "bug: expected a level 1 term"
      end
  | FunType (name, param_ty, body_ty) ->
      let param_ty = translate1 env param_ty in
      let body_ty = translate1 (bind_entry0 env) body_ty in
      FunType00 (name, param_ty, body_ty)
  | FunLit (name, param_ty, body) ->
      begin match translate_ty env param_ty with
      | `Tm2 param_ty ->
          begin match translate_tm (bind_entry1 env) body with
          | `Tm1 body -> FunLit11 (name, param_ty, body)
          | `Tm0 body -> FunLit10 (name, param_ty, body)
          end
      | `Tm1 param_ty ->
          FunLit01 (name, param_ty, translate1 (bind_entry0 env) body)
      end
  | FunApp (head, arg) ->
      begin match translate_tm env arg with
      | `Tm1 arg -> FunApp11 (translate1 env head, arg)
      | `Tm0 arg -> FunApp01 (translate1 env head, arg)
      end
  | _ -> failwith "bug: expected a level 1 term"

and translate0 env : Core.Syntax.tm -> Stratified.Syntax.tm0 =
  function
  | Let (name, def, body) ->
      begin match translate_tm env def with
      | `Tm1 def -> Let10 (name, def, translate0 (bind_entry1 env) body)
      | `Tm0 def -> Let00 (name, def, translate0 (bind_entry0 env) body)
      end
  | Var x ->
      begin match Env.get_index x env.levels with
      | Level1 _ -> failwith "bug: expected a level 0 term"
      | Level0 level -> Var0 (Env.level_to_index env.size0 level)
      end
  | Ann (expr, ty) ->
      Ann0 (translate0 env expr, translate1 env ty)
  | FunLit (name, param_ty, body) ->
      let param_ty = translate1 env param_ty in
      let body = translate0 (bind_entry0 env) body in
      FunLit00 (name, param_ty, body)
  | FunApp (head, arg) ->
      begin match translate_tm env head with
      | `Tm1 head -> FunApp10 (head, translate1 env arg)
      | `Tm0 head -> FunApp00 (head, translate0 env arg)
      end
  | _ -> failwith "bug: expected a level 0 term"
