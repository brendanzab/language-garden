(** {0 Stratified language} *)


(** {0 Names} *)

(** These names are used as hints for pretty printing binders and variables,
but don’t impact the equality of terms. *)
type name = string option


(** Variable namespaces *)
module Ns = struct

  (** Level 1 term bindings *)
  type tm1

  (** Level 0 term bindings *)
  type tm0

end


(** Syntax of the stratified language *)
module Syntax = struct

  (** {1 Types} *)

  (** Level 1 types, inhabiting the [Type 1] universe. *)
  type ty1 = tm2

  (** Level 0 types, inhabiting the [Type 0] universe. *)
  and ty0 = tm1


  (** {1 Terms} *)

  (** These are numbered by the level of the universe that is needed to contain
      the types that they inhabit. *)

  and tm2 =
    | Univ0                           (** Universe of small types (ie. [Type 0]) *)
    | FunType11 of name * ty1 * ty1   (** [Type 1] to [Type 1] function types *)
    | FunType01 of name * ty0 * ty1   (** [Type 0] to [Type 1] function types *)
  and tm1 =
    | Let11 of name * tm1 * tm1       (** [Type 1] let binding *)
    | Let01 of name * tm0 * tm1       (** [Type 0] let binding *)
    | Ann1 of tm1 * ty1
    | Var1 of Ns.tm1 Env.index
    | FunLit11 of name * ty1 * tm1    (** [Type 1] to [Type 1] function literals *)
    | FunLit01 of name * ty0 * tm1    (** [Type 0] to [Type 1] function literals *)
    | FunApp11 of tm1 * tm1           (** [Type 1] to [Type 1] function application *)
    | FunApp01 of tm1 * tm0           (** [Type 0] to [Type 1] function application *)
    | FunType00 of name * ty0 * ty0   (** [Type 0] to [Type 0] function types *)
  and tm0 =
    | Let10 of name * tm1 * tm0       (** [Type 1] let binding *)
    | Let00 of name * tm0 * tm0       (** [Type 0] let binding *)
    | Ann0 of tm0 * ty0
    | Var0 of Ns.tm0 Env.index
    | FunLit00 of name * ty0 * tm0    (** [Type 0] to [Type 0] function literals *)
    | FunApp00 of tm0 * tm0           (** [Type 0] to [Type 0] function application *)

end


(** Semantics of the stratified language *)
module Semantics = struct

  (** {1 Semantic domain} *)

  (** {2 Types} *)

  type vty1 = vtm2
  and vty0 = vtm1

  (** {2 Values} *)

  and vtm2 =
    | Univ0
    | FunType11 of name * vty1 * (vtm1 -> vty1)
    | FunType01 of name * vty0 * (vtm0 -> vty1)
  and vtm1 =
    | Neu1 of neu1
    | FunLit11 of name * vty1 * (vtm1 -> vtm1)
    | FunLit01 of name * vty0 * (vtm0 -> vtm1)
    | FunType00 of name * vty0 * (vtm0 -> vty0)
  and vtm0 =
    | Neu0 of neu0
    | FunLit00 of name * vty0 * (vtm0 -> vtm0)

  (** {2 Neutral terms} *)

  and neu1 =
    | Var1 of Ns.tm1 Env.level
    | FunApp11 of neu1 * vtm1
    | FunApp01 of neu1 * vtm0
  and neu0 =
    | Var0 of Ns.tm0 Env.level
    | FunApp00 of neu0 * vtm0


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Environments} *)

  type env = {
    tm1s : (Ns.tm1, vtm1) Env.t;   (** Level 1 bindings *)
    tm0s : (Ns.tm0, vtm0) Env.t;   (** Level 0 bindings *)
  }

  let bind1 t env =
    { env with tm1s = Env.bind_entry t env.tm1s }

  let bind0 t env =
    { env with tm0s = Env.bind_entry t env.tm0s }


  (** {1 Eliminators} *)

  let app11 head arg =
    match head with
    | Neu1 neu -> Neu1 (FunApp11 (neu, arg))
    | FunLit11 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app01 head arg =
    match head with
    | Neu1 neu -> Neu1 (FunApp01 (neu, arg))
    | FunLit01 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app00 head arg =
    match head with
    | Neu0 neu -> Neu0 (FunApp00 (neu, arg))
    | FunLit00 (_, _, body) -> body arg


  (** {1 Evaluation} *)

  let rec eval2 env : Syntax.tm2 -> vtm2 =
    function
    | Univ0 -> Univ0
    | FunType11 (name, param_ty, body_ty) ->
        let param_ty = eval2 env param_ty in
        let body_ty x = eval2 (bind1 x env) body_ty in
        FunType11 (name, param_ty, body_ty)
    | FunType01 (name, param_ty, body_ty) ->
        let param_ty = eval1 env param_ty in
        let body_ty x = eval2 (bind0 x env) body_ty in
        FunType01 (name, param_ty, body_ty)

  and eval1 env : Syntax.tm1 -> vtm1 =
    function
    | Let11 (_, def, body) -> eval1 (bind1 (eval1 env def) env) body
    | Let01 (_, def, body) -> eval1 (bind0 (eval0 env def) env) body
    | Ann1 (t, _) -> eval1 env t
    | Var1 x -> Env.get_index x env.tm1s
    | FunLit11 (name, param_ty, body) ->
        let param_ty = eval2 env param_ty in
        let body x = eval1 (bind1 x env) body in
        FunLit11 (name, param_ty, body)
    | FunLit01 (name, param_ty, body) ->
        let param_ty = eval1 env param_ty in
        let body x = eval1 (bind0 x env) body in
        FunLit01 (name, param_ty, body)
    | FunApp11 (head, arg) ->
        app11 (eval1 env head) (eval1 env arg)
    | FunApp01 (head, arg) ->
        app01 (eval1 env head) (eval0 env arg)
    | FunType00 (name, param_ty, body_ty) ->
        let param_ty = eval1 env param_ty in
        let body_ty x = eval1 (bind0 x env) body_ty in
        FunType00 (name, param_ty, body_ty)

  and eval0 env : Syntax.tm0 -> vtm0 =
    function
    | Let10 (_, def, body) -> eval0 (bind1 (eval1 env def) env) body
    | Let00 (_, def, body) -> eval0 (bind0 (eval0 env def) env) body
    | Ann0 (t, _) -> eval0 env t
    | Var0 x -> Env.get_index x env.tm0s
    | FunLit00 (name, param_ty, body) ->
        let param_ty = eval1 env param_ty in
        let body x = eval0 (bind0 x env) body in
        FunLit00 (name, param_ty, body)
    | FunApp00 (head, arg) ->
        app00 (eval0 env head) (eval0 env arg)

end
