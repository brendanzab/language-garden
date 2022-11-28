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
    | Univ                        (** The universe of ‘small’ types (i.e. the type of types) *)
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


(** Validation (type checking) of core terms *)
module Validation = struct

  type ctx = {
    size : Ns.tm Env.size;
    tys : (Ns.tm, Semantics.vty) Env.t;
    tms : (Ns.tm, Semantics.vty) Env.t;
  }

  let next_var ctx =
    Semantics.Neu (Var (Env.next_level ctx.size))

  let bind_def ty tm ctx = {
    size = ctx.size |> Env.bind_level;
    tys = ctx.tys |> Env.bind_entry ty;
    tms = ctx.tms |> Env.bind_entry tm;
  }

  let bind_param ty ctx =
    ctx |> bind_def ty (next_var ctx)


  (* Type errors raised when validating the core language *)
  exception Error of string


  (** Check that a term conforms to a given type. *)
  let rec check ctx (tm : Syntax.tm) (expected_ty : Semantics.vty) =
    let ty = synth ctx tm in
    if Semantics.is_convertible ctx.size (ty, expected_ty) then () else
      raise (Error "mismatched types")

  (** Synthesize the type of a term *)
  and synth ctx : Syntax.tm -> Semantics.vty =
    function
    | Let (_, def, body) ->
        let def_ty = synth ctx def in
        let def' = Semantics.eval ctx.tms def in
        synth (ctx |> bind_def def_ty def') body
    | Ann (expr, ty) ->
        is_ty ctx ty;
        let ty' = Semantics.eval ctx.tms ty in
        check ctx expr ty';
        ty'
    | Var x -> Env.get_index x ctx.tys

    (* There’s no type large enough to contain large universes in the core
       language, so raise an error here *)
    | Univ -> raise (Error "cannot synthesize the type of types")
    (* Introduction rule for small function universes. Larger functions are
       handled in the [is_type] function, but there are restrictions on where
       these can appear. *)
    | FunType (_, param_ty, body_ty) ->
        check ctx param_ty Univ;
        let param_ty' = Semantics.eval ctx.tms param_ty in
        check (ctx |> bind_param param_ty') body_ty Univ;
        Univ
    (* Introduction rule for functions *)
    | FunLit (name, param_ty, body) ->
        is_ty ctx param_ty;
        let param_ty' = Semantics.eval ctx.tms param_ty in
        let body_ty = synth (ctx |> bind_param param_ty') body in
        let fun_ty = Syntax.FunType (name, param_ty, Semantics.quote ctx.size body_ty) in
        Semantics.eval ctx.tms fun_ty
    (* Elimination rule for functions *)
    | FunApp (head, arg) ->
        match synth ctx head with
        | FunType (_, param_ty, body_ty) ->
            check ctx arg (Lazy.force param_ty);
            body_ty (Semantics.eval ctx.tms arg)
        | _ -> raise (Error "expected a function type")

  (** Check if the term is a small or a large type. Terms that are typable with
      this function, but not [check] or [synth] are usually restricted to only
      appear in type annotations. *)
  and is_ty ctx : Syntax.tm -> unit =
    function
    (* The type of universes is a type *)
    | Univ -> ()
    (* Function type formation for small and large function types. *)
    | FunType (_, param_ty, body_ty) ->
        is_ty ctx param_ty;
        let param_ty' = Semantics.eval ctx.tms param_ty in
        is_ty (ctx |> bind_param param_ty') body_ty
    (* Terms that inhabit the universe of small types can be treated a types,
       effectively ‘coercing’ them to types. *)
    | tm -> check ctx tm Univ

end
