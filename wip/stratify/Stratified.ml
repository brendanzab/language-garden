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

  type ty1 = tm2
  and ty0 = tm1

  and ty =
    | Ty1 of ty1
    | Ty0 of ty0


  (** {1 Terms} *)

  and tm2 =
    | Univ
    | FunType1 of name * ty * ty1
  and tm1 =
    | Let1 of name * tm * tm1
    | Var1 of Ns.tm1 Env.index
    | FunLit1 of name * ty * tm1
    | FunApp1 of tm1 * tm
    | FunType0 of name * ty0 * ty0
  and tm0 =
    | Let0 of name * tm * tm0
    | Var0 of Ns.tm0 Env.index
    | FunLit0 of name * ty * tm0
    | FunApp0 of tm0 * tm

  and tm =
    | Tm1 of tm1
    | Tm0 of tm0

end


(** Semantics of the stratified language *)
module Semantics = struct

  (** {1 Semantic domain} *)

  (** {2 Types} *)

  type vty1 = vtm2
  and vty0 = vtm1

  and vty =
    | Ty1 of vty1
    | Ty0 of vty0

  (** {2 Values} *)

  and vtm2 =
    | Univ
    | FunType1 of name * vty * (vtm -> vty1)
  and vtm1 =
    | Neu1 of neu1
    | FunLit1 of name * vty * (vtm -> vtm1)
    | FunType0 of name * vty0 * (vtm -> vty0)
  and vtm0 =
    | Neu0 of neu0
    | FunLit0 of name * vty * (vtm -> vtm0)

  and vtm =
    | Tm1 of vtm1
    | Tm0 of vtm0

  (** {2 Neutral terms} *)

  and neu1 =
    | Var1 of Ns.tm1 Env.level
    | FunApp1 of neu1 * vtm
  and neu0 =
    | Var0 of Ns.tm0 Env.level
    | FunApp0 of neu0 * vtm


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Environments} *)

  type env = {
    tm1s : (Ns.tm1, vtm1) Env.t;   (** Level 1 bindings *)
    tm0s : (Ns.tm0, vtm0) Env.t;   (** Level 0 bindings *)
  }

  let bind vtm env =
    match vtm with
    | Tm1 t -> { env with tm1s = Env.bind_entry t env.tm1s }
    | Tm0 t -> { env with tm0s = Env.bind_entry t env.tm0s }


  (** {1 Eliminators} *)

  let app1 head arg =
    match head with
    | Neu1 neu -> Neu1 (FunApp1 (neu, arg))
    | FunLit1 (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")

  let app0 head arg =
    match head with
    | Neu0 neu -> Neu0 (FunApp0 (neu, arg))
    | FunLit0 (_, _, body) -> body arg


  (** {1 Evaluation} *)

  let rec eval2 env : Syntax.tm2 -> vtm2 =
    function
    | Univ -> Univ
    | FunType1 (name, param_ty, body_ty) ->
        let param_ty = eval_ty env param_ty in
        let body_ty x = eval2 (bind x env) body_ty in
        FunType1 (name, param_ty, body_ty)

  and eval1 env : Syntax.tm1 -> vtm1 =
    function
    | Let1 (_, def, body) ->
        eval1 (bind (eval_tm env def) env) body
    | Var1 x -> Env.get_index x env.tm1s
    | FunLit1 (name, param_ty, body) ->
        let param_ty = eval_ty env param_ty in
        let body x = eval1 (bind x env) body in
        FunLit1 (name, param_ty, body)
    | FunApp1 (head, arg) ->
        app1 (eval1 env head) (eval_tm env arg)
    | FunType0 (name, param_ty, body_ty) ->
        let param_ty = eval1 env param_ty in
        let body_ty x = eval1 (bind x env) body_ty in
        FunType0 (name, param_ty, body_ty)

  and eval0 env : Syntax.tm0 -> vtm0 =
    function
    | Let0 (_, def, body) ->
        eval0 (bind (eval_tm env def) env) body
    | Var0 x -> Env.get_index x env.tm0s
    | FunLit0 (name, param_ty, body) ->
        let param_ty = eval_ty env param_ty in
        let body x = eval0 (bind x env) body in
        FunLit0 (name, param_ty, body)
    | FunApp0 (head, arg) ->
        app0 (eval0 env head) (eval_tm env arg)

  and eval_tm env : Syntax.tm -> vtm =
    function
    | Tm1 t -> Tm1 (eval1 env t)
    | Tm0 t -> Tm0 (eval0 env t)

  and eval_ty env : Syntax.ty -> vty =
    function
    | Ty1 t -> Ty1 (eval2 env t)
    | Ty0 t -> Ty0 (eval1 env t)

end
