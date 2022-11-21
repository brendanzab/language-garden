(** {0 Core language} *)


(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


(** Variable namespaces *)
module Ns = struct

  (** Local term bindings *)
  type tm

end


(** Syntax of the core language *)
module Syntax = struct

  (** Types *)
  type ty = tm

  (** Terms *)
  and tm =
    | Let of name * tm * tm       (** Let bindings (for sharing definitions inside terms) *)
    | Ann of tm * ty              (** Terms annotated with types *)
    | Var of Ns.tm Env.index      (** Variables *)
    | Univ                        (** Universe (i.e. the type of types) *)
    | FunType of name * ty * ty   (** Dependent function types *)
    | FunLit of name * ty * tm    (** Function literals (i.e. lambda expressions) *)
    | FunApp of tm * tm           (** Function application *)

end


(** Semantics of the core language *)
module Semantics = struct

  (** {1 Semantic domain} *)

  (** Types *)
  type vty = vtm

  (** Terms in weak head normal form *)
  and vtm =
    | Neu of neu
    | Univ
    | FunType of name * vty Lazy.t * (vtm -> vty)
    | FunLit of name * vty Lazy.t * (vtm -> vtm)

  (** Neutral terms *)
  and neu =
    | Var of Ns.tm Env.level
    | FunApp of neu * vtm Lazy.t


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Eliminators} *)

  let app head arg =
    match head with
    | Neu neu -> Neu (FunApp (neu, Lazy.from_val arg))
    | FunLit (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")


  (** {1 Evaluation} *)

  let rec eval env = function
    | Syntax.Let (_, def, body) -> eval (Env.bind_entry (eval env def) env) body
    | Syntax.Ann (tm, _) -> eval env tm
    | Syntax.Var x -> Env.get_index x env
    | Syntax.Univ ->  Univ
    | Syntax.FunType (name, param_ty, body_ty) ->
        let param_ty = Lazy.from_fun (fun () -> eval env param_ty) in
        let body_ty x = eval (Env.bind_entry x env) body_ty in
        FunType (name, param_ty, body_ty)
    | Syntax.FunLit (name, param_ty, body) ->
        let param_ty = Lazy.from_fun (fun () -> eval env param_ty) in
        let body x = eval (Env.bind_entry x env) body in
        FunLit (name, param_ty, body)
    | Syntax.FunApp (head, arg) -> app (eval env head) (eval env arg)


  (** {1 Quotation} *)

  let rec quote size = function
    | Neu neu -> quote_neu size neu
    | Univ -> Syntax.Univ
    | FunType (name, param_ty, body_ty) ->
        let x = Neu (Var (Env.next_level size)) in
        let param_ty = quote size (Lazy.force param_ty) in
        let body_ty = quote (Env.bind_level size) (body_ty x) in
        Syntax.FunType (name, param_ty, body_ty)
    | FunLit (name, param_ty, body) ->
        let x = Neu (Var (Env.next_level size)) in
        let param_ty = quote size (Lazy.force param_ty) in
        let body = quote (Env.bind_level size) (body x) in
        Syntax.FunLit (name, param_ty, body)
  and quote_neu size = function
    | Var level -> Syntax.Var (Env.level_to_index size level)
    | FunApp (neu, arg) -> Syntax.FunApp (quote_neu size neu, quote size (Lazy.force arg))


  (** {1 Normalisation} *)

  let normalise size tms tm : Syntax.tm =
    quote size (eval tms tm)


  (** {1 Conversion Checking} *)

  let rec is_convertible size = function
    | Neu neu1, Neu neu2 -> is_convertible_neu size (neu1, neu2)
    | Univ, Univ -> true
    | FunType (_, param_ty1, body_ty1), FunType (_, param_ty2, body_ty2) ->
        let x = Neu (Var (Env.next_level size)) in
        is_convertible size (Lazy.force param_ty1, Lazy.force param_ty2)
          && is_convertible (Env.bind_level size) (body_ty1 x, body_ty2 x)
    | FunLit (_, param_ty1, body1), FunLit (_, param_ty2, body2) ->
        let x = Neu (Var (Env.next_level size)) in
        is_convertible size (Lazy.force param_ty1, Lazy.force param_ty2)
          && is_convertible (Env.bind_level size) (body1 x, body2 x)
    (* Eta for functions *)
    | FunLit (_, _, body), fun_tm | fun_tm, FunLit (_, _, body)  ->
        let x = Neu (Var (Env.next_level size)) in
        is_convertible size (body x, app fun_tm x)
    | _, _ -> false
  and is_convertible_neu size = function
    | Var level1, Var level2 -> level1 = level2
    | FunApp (neu1, arg1), FunApp (neu2, arg2)  ->
        is_convertible_neu size (neu1, neu2)
          && is_convertible size (Lazy.force arg1, Lazy.force arg2)
    | _, _ -> false

end
