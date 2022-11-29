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
    | FunType10 of name * ty1 * ty0   (** [Type 1] to [Type 0] function types *)
  and tm1 =
    | Let11 of name * tm1 * tm1       (** [Type 1] let binding *)
    | Let01 of name * tm0 * tm1       (** [Type 0] let binding *)
    | Ann1 of tm1 * ty1
    | Var1 of Ns.tm1 Env.index
    | FunLit11 of name * ty1 * tm1    (** [Type 1] to [Type 1] function literals *)
    | FunApp11 of tm1 * tm1           (** [Type 1] to [Type 1] function application *)
    | FunLit01 of name * ty0 * tm1    (** [Type 0] to [Type 1] function literals *)
    | FunApp01 of tm1 * tm0           (** [Type 0] to [Type 1] function application *)
    | FunLit10 of name * ty1 * tm0    (** [Type 1] to [Type 0] function literals *)
    | FunType00 of name * ty0 * ty0   (** [Type 0] to [Type 0] function types *)
  and tm0 =
    | Let10 of name * tm1 * tm0       (** [Type 1] let binding *)
    | Let00 of name * tm0 * tm0       (** [Type 0] let binding *)
    | Ann0 of tm0 * ty0
    | Var0 of Ns.tm0 Env.index
    | FunApp10 of tm1 * tm1           (** [Type 1] to [Type 0] function application *)
    | FunLit00 of name * ty0 * tm0    (** [Type 0] to [Type 0] function literals *)
    | FunApp00 of tm0 * tm0           (** [Type 0] to [Type 0] function application *)

  (** We have four different function types:

      - [FunType11]: {v (Type 1, Type 1, Type 1) v} (types parameterised by types)
      - [FunType10]: {v (Type 0, Type 1, Type 1) v} (types parameterised by terms)
      - [FunType01]: {v (Type 1, Type 0, Type 1) v} (terms paremeterised by types)
      - [FunType00]: {v (Type 0, Type 0, Type 0) v} (terms paremeterised by terms)

      These correspond to each function rule in the core language. For example:

      - [FunType11]: {v                         ⊢ (x : Type) -> Type v}
      - [FunType10]: {v A : Type                ⊢ (x : Type) -> A v}
      - [FunType01]: {v A : Type                ⊢ (x : A) -> Type v}
      - [FunType00]: {v A : Type, B : A -> Type ⊢ (x : A) -> B x v}
  *)

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
    | FunType10 of name * vty1 * (vtm1 -> vty0)
  and vtm1 =
    | Neu1 of neu1
    | FunLit11 of name * vty1 * (vtm1 -> vtm1)
    | FunLit01 of name * vty0 * (vtm0 -> vtm1)
    | FunLit10 of name * vty1 * (vtm1 -> vtm0)
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
    | FunApp10 of neu1 * vtm1
    | FunApp00 of neu0 * vtm0

  (** Note that the [FunApp10] neutral is a bit peculiar as it allows us to
      construct a level 0 neutral from a level 1 neutral! *)


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Environments} *)

  type env = {
    tm1s : (Ns.tm1, vtm1) Env.t;   (** Level 1 bindings *)
    tm0s : (Ns.tm0, vtm0) Env.t;   (** Level 0 bindings *)
  }

  let bind1 (t : vtm1) env =
    { env with tm1s = Env.bind_entry t env.tm1s }

  let bind0 (t : vtm0) env =
    { env with tm0s = Env.bind_entry t env.tm0s }


  (** {1 Eliminators} *)

  let app11 (head : vtm1) (arg : vtm1) : vtm1 =
    match head with
    | Neu1 neu -> Neu1 (FunApp11 (neu, arg))
    | FunLit11 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app01 (head : vtm1) (arg : vtm0) : vtm1 =
    match head with
    | Neu1 neu -> Neu1 (FunApp01 (neu, arg))
    | FunLit01 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app10 (head : vtm1) (arg : vtm1) : vtm0 =
    match head with
    | Neu1 neu -> Neu0 (FunApp10 (neu, arg))
    | FunLit10 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app00 (head : vtm0) (arg : vtm0) : vtm0 =
    match head with
    | Neu0 neu -> Neu0 (FunApp00 (neu, arg))
    | FunLit00 (_, _, body) -> body arg


  (** {1 Evaluation} *)

  let rec eval2 env : Syntax.tm2 -> vtm2 =
    function
    | Univ0 -> Univ0
    | FunType11 (x, param_ty, body_ty) ->
        FunType11 (x, eval2 env param_ty, fun x -> eval2 (bind1 x env) body_ty)
    | FunType01 (x, param_ty, body_ty) ->
        FunType01 (x, eval1 env param_ty, fun x -> eval2 (bind0 x env) body_ty)
    | FunType10 (x, param_ty, body_ty) ->
        FunType10 (x, eval2 env param_ty, fun x -> eval1 (bind1 x env) body_ty)

  and eval1 env : Syntax.tm1 -> vtm1 =
    function
    | Let11 (_, def, body) -> eval1 (bind1 (eval1 env def) env) body
    | Let01 (_, def, body) -> eval1 (bind0 (eval0 env def) env) body
    | Ann1 (t, _) -> eval1 env t
    | Var1 x -> Env.get_index x env.tm1s
    | FunLit11 (x, param_ty, body) ->
        FunLit11 (x, eval2 env param_ty, fun x -> eval1 (bind1 x env) body)
    | FunApp11 (head, arg) -> app11 (eval1 env head) (eval1 env arg)
    | FunLit01 (x, param_ty, body) ->
        FunLit01 (x, eval1 env param_ty, fun x -> eval1 (bind0 x env) body)
    | FunApp01 (head, arg) -> app01 (eval1 env head) (eval0 env arg)
    | FunLit10 (x, param_ty, body) ->
        FunLit10 (x, eval2 env param_ty, fun x -> eval0 (bind1 x env) body)
    | FunType00 (x, param_ty, body_ty) ->
        FunType00 (x, eval1 env param_ty, fun x -> eval1 (bind0 x env) body_ty)

  and eval0 env : Syntax.tm0 -> vtm0 =
    function
    | Let10 (_, def, body) -> eval0 (bind1 (eval1 env def) env) body
    | Let00 (_, def, body) -> eval0 (bind0 (eval0 env def) env) body
    | Ann0 (t, _) -> eval0 env t
    | Var0 x -> Env.get_index x env.tm0s
    | FunApp10 (head, arg) -> app10 (eval1 env head) (eval1 env arg)
    | FunLit00 (x, param_ty, body) ->
        FunLit00 (x, eval1 env param_ty, fun x -> eval0 (bind0 x env) body)
    | FunApp00 (head, arg) -> app00 (eval0 env head) (eval0 env arg)

end
