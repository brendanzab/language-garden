(** Translation context *)
module Context = struct
  (** The stratified syntax has two namespaces: one for level 1 terms, and another
      for level 0 terms. The translation context keeps track of the number of
      entries bound for each namespace, with a mapping from bindings in the core
      language bindings in the stratified language.

      Bindings are stored as levels, ready to be converted back to indices at
      the appropriate binding depth. This allows us to add new bindings without
      shifting.
  *)

  type level =
    | Level1 of Stratified.Ns.tm1 Env.level
    | Level0 of Stratified.Ns.tm0 Env.level

  type t = {
    (* Current size of the stratified contexts *)
    size1 : Stratified.Ns.tm1 Env.size;
    size0 : Stratified.Ns.tm0 Env.size;
    (* Mappings from the core language bindings to stratified bindings. *)
    levels : (Core.Ns.tm, level) Env.t;
  }

  let empty = {
    size1 = Env.empty_size;
    size0 = Env.empty_size;
    levels = Env.empty;
  }

  (** Add a level 1 binding to the context *)
  let define1 ctx = {
    ctx with
    size1 = ctx.size1 |> Env.bind_level;
    levels = ctx.levels |> Env.bind_entry (Level1 (Env.next_level ctx.size1));
  }

  (** Add a level 0 binding to the context *)
  let define0 ctx = {
    ctx with
    size0 = ctx.size0 |> Env.bind_level;
    levels = ctx.levels |> Env.bind_entry (Level0 (Env.next_level ctx.size0));
  }

end

type tm = [
  | `Tm2 of Stratified.Syntax.tm2
  | `Tm1 of Stratified.Syntax.tm1
  | `Tm0 of Stratified.Syntax.tm0
]

exception Error of string

let rec translate ctx : Core.Syntax.tm -> tm =
  function
  | Let (name, def, body) ->
      begin match translate_tm ctx def with
      | `Tm1 def ->
          begin match translate_tm (Context.define1 ctx) body with
          | `Tm1 body -> `Tm1 (Let11 (name, def, body))
          | `Tm0 body -> `Tm0 (Let10 (name, def, body))
          end
      | `Tm0 def ->
          begin match translate_tm (Context.define0 ctx) body with
          | `Tm1 body -> `Tm1 (Let01 (name, def, body))
          | `Tm0 body -> `Tm0 (Let00 (name, def, body))
          end
      end
  | Ann (expr, ty) ->
      begin match translate_ty ctx ty with
      | `Tm2 ty -> `Tm1 (Ann1 (translate1 ctx expr, ty))
      | `Tm1 ty -> `Tm0 (Ann0 (translate0 ctx expr, ty))
      end
  | Var x ->
      begin match Env.lookup x ctx.levels with
      | Level1 level -> `Tm1 (Var1 (Env.level_to_index ctx.size1 level))
      | Level0 level -> `Tm0 (Var0 (Env.level_to_index ctx.size0 level))
      end
  | Univ L0 -> `Tm2 Univ0
  | Univ L1 -> raise (Error "bug: universe exceeds size of target language")
  | Fun_type (name, param_ty, body_ty) ->
      begin match translate_ty ctx param_ty with
      | `Tm2 param_ty ->
          begin match translate_ty (Context.define1 ctx) body_ty with
          | `Tm2 body_ty -> `Tm2 (Fun_type11 (name, param_ty, body_ty))
          | `Tm1 body_ty -> `Tm2 (Fun_type10 (name, param_ty, body_ty))
          end
      | `Tm1 param_ty ->
          begin match translate_ty (Context.define0 ctx) body_ty with
          | `Tm2 body_ty -> `Tm2 (Fun_type01 (name, param_ty, body_ty))
          | `Tm1 body_ty -> `Tm1 (Fun_type00 (name, param_ty, body_ty))
          end
      end
  | Fun_lit (name, param_ty, body) ->
      begin match translate_ty ctx param_ty with
      | `Tm2 param_ty ->
          begin match translate_tm (Context.define1 ctx) body with
          | `Tm1 body -> `Tm1 (Fun_lit11 (name, param_ty, body))
          | `Tm0 body -> `Tm1 (Fun_lit10 (name, param_ty, body))
          end
      | `Tm1 param_ty ->
          begin match translate_tm (Context.define0 ctx) body with
          | `Tm1 body -> `Tm1 (Fun_lit01 (name, param_ty, body))
          | `Tm0 body -> `Tm0 (Fun_lit00 (name, param_ty, body))
          end
      end
  | Fun_app (head, arg) ->
      begin match translate_tm ctx head with
      | `Tm1 _ ->
          (*  This could either be a [Fun_app11], [Fun_app01], or [Fun_app10], but
              we can’t tell from the term alone. See [translate1] and
              [translate0] for cases where this is not ambiguous.

              I’m not sure if this handles all of the terms that pass validation
              in the [Core.Validation] functions. Perhaps a type-directed
              translation could help make this feel less ad-hoc? Not sure.
          *)
          raise (Error "bug: ambiguous function application")
      | `Tm0 head -> `Tm0 (Fun_app00 (head, translate0 ctx arg))
      end


(** {1 Translation functions specialised to ranges of term levels} *)

(** The following functions allow us to avoid some additional checks in when we
    know a bit more about the term we’re expecting. *)

(** Translate a term that can be given a type (i.e. that can appear on the left
    of the typing operator). *)
and translate_tm ctx tm =
  match translate (Context.define0 ctx) tm with
  | `Tm1  _ | `Tm0 _ as tm -> tm
  | `Tm2 _ -> raise (Error "bug: level 2 terms are too large to be typable")

(** Translate a term that can be used as a type in the target language (i.e.
    that can appear on the right of the typing operator). *)
and translate_ty ctx tm =
  match translate (Context.define0 ctx) tm with
  | `Tm2  _ | `Tm1 _ as tm -> tm
  | `Tm0 _ -> raise (Error "bug: level 0 terms are too small to contain types")


(** {1 Translation functions specialised to known term levels} *)

(** The following translation functions are specialised to specific term levels.
    The idea is to try to supply more information about the expected level of
    the term, in an attempt to avoid ambiguous function applications.
*)

(* FIXME: The following functions feel incredibly ad hoc?? Not sure if there’s
   a nicer way to account for all these cases? *)

and translate2 ctx : Core.Syntax.tm -> Stratified.Syntax.tm2 =
  function
  | Univ L0 -> Univ0
  | Fun_type (name, param_ty, body_ty) ->
      begin match translate_ty ctx param_ty with
      | `Tm2 param_ty ->
          begin match translate_ty (Context.define1 ctx) body_ty with
          | `Tm2 body_ty -> Fun_type11 (name, param_ty, body_ty)
          | `Tm1 body_ty -> Fun_type10 (name, param_ty, body_ty)
          end
      | `Tm1 param_ty ->
          Fun_type01 (name, param_ty, translate2 (Context.define0 ctx) body_ty)
      end
  | _ -> raise (Error "bug: expected a level 2 term")

and translate1 ctx : Core.Syntax.tm -> Stratified.Syntax.tm1 =
  function
  | Let (name, def, body) ->
      begin match translate_tm ctx def with
      | `Tm1 def -> Let11 (name, def, translate1 (Context.define1 ctx) body)
      | `Tm0 def -> Let01 (name, def, translate1 (Context.define0 ctx) body)
      end
  | Ann (expr, ty) ->
      Ann1 (translate1 ctx expr, translate2 ctx ty)
  | Var x ->
      begin match Env.lookup x ctx.levels with
      | Level1 level -> Var1 (Env.level_to_index ctx.size1 level)
      | Level0 _ -> raise (Error "bug: expected a level 1 term")
      end
  | Fun_type (name, param_ty, body_ty) ->
      let param_ty = translate1 ctx param_ty in
      let body_ty = translate1 (Context.define0 ctx) body_ty in
      Fun_type00 (name, param_ty, body_ty)
  | Fun_lit (name, param_ty, body) ->
      begin match translate_ty ctx param_ty with
      | `Tm2 param_ty ->
          begin match translate_tm (Context.define1 ctx) body with
          | `Tm1 body -> Fun_lit11 (name, param_ty, body)
          | `Tm0 body -> Fun_lit10 (name, param_ty, body)
          end
      | `Tm1 param_ty ->
          Fun_lit01 (name, param_ty, translate1 (Context.define0 ctx) body)
      end
  | Fun_app (head, arg) ->
      begin match translate_tm ctx arg with
      | `Tm1 arg -> Fun_app11 (translate1 ctx head, arg)
      | `Tm0 arg -> Fun_app01 (translate1 ctx head, arg)
      end
  | _ -> raise (Error "bug: expected a level 1 term")

and translate0 ctx : Core.Syntax.tm -> Stratified.Syntax.tm0 =
  function
  | Let (name, def, body) ->
      begin match translate_tm ctx def with
      | `Tm1 def -> Let10 (name, def, translate0 (Context.define1 ctx) body)
      | `Tm0 def -> Let00 (name, def, translate0 (Context.define0 ctx) body)
      end
  | Var x ->
      begin match Env.lookup x ctx.levels with
      | Level1 _ -> raise (Error "bug: expected a level 0 term")
      | Level0 level -> Var0 (Env.level_to_index ctx.size0 level)
      end
  | Ann (expr, ty) ->
      Ann0 (translate0 ctx expr, translate1 ctx ty)
  | Fun_lit (name, param_ty, body) ->
      let param_ty = translate1 ctx param_ty in
      let body = translate0 (Context.define0 ctx) body in
      Fun_lit00 (name, param_ty, body)
  | Fun_app (head, arg) ->
      begin match translate_tm ctx head with
      | `Tm1 head -> Fun_app10 (head, translate1 ctx arg)
      | `Tm0 head -> Fun_app00 (head, translate0 ctx arg)
      end
  | _ -> raise (Error "bug: expected a level 0 term")
