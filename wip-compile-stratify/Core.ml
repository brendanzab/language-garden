(** {0 Core language} *)


(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


(** Universe levels *)
module Level = struct

  (** Universe levels *)
  type t =
    | L0  (** Small types *)
    | L1  (** Large types *)

  let max l1 l2 =
    match l1, l2 with
    | L0, L0 -> L0
    | L1, _ | _, L1 -> L1

end


(** Variable namespaces *)
module Ns = struct

  (** Local term bindings *)
  type tm

end


(** Syntax of the core language *)
module Syntax = struct
  (** The core language corresponds to a pure type system instantiated with the
      following sorts, axioms, and rules:

      {v
        Level := { 0, 1 }

        S := { Type l | l ∈ Level }
        A := { (Type 0 : Type 1) }
        R := { (Type l₁, Type l₂, Type (max l₁ l₂)) | l₁, l₂ ∈ Level }
      v}

      This gives us a dependently typed lambda calculus with two predicative
      universes.
  *)

  (** Types *)
  type ty = tm

  (** Terms *)
  and tm =
    | Let of name * tm * tm       (** Let bindings (for sharing definitions inside terms) *)
    | Ann of tm * ty              (** Terms annotated with types *)
    | Var of Ns.tm Env.index      (** Variables *)
    | Univ of Level.t             (** Universe (i.e. the type of types) *)
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
    | Univ of Level.t
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

  let app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu neu -> Neu (FunApp (neu, Lazy.from_val arg))
    | FunLit (_, _, body) -> body arg
    | _ -> raise (Error "invalid application")


  (** {1 Evaluation} *)

  let rec eval env : Syntax.tm -> vtm = function
    | Syntax.Let (_, def, body) -> eval (Env.bind_entry (eval env def) env) body
    | Syntax.Ann (tm, _) -> eval env tm
    | Syntax.Var x -> Env.get_index x env
    | Syntax.Univ l ->  Univ l
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

  let rec quote size : vtm -> Syntax.tm = function
    | Neu neu -> quote_neu size neu
    | Univ l -> Syntax.Univ l
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
  and quote_neu size : neu -> Syntax.tm = function
    | Var level -> Syntax.Var (Env.level_to_index size level)
    | FunApp (neu, arg) -> Syntax.FunApp (quote_neu size neu, quote size (Lazy.force arg))


  (** {1 Normalisation} *)

  let normalise env (tm : Syntax.tm) : Syntax.tm =
    quote (Env.size env) (eval env tm)


  (** {1 Conversion Checking} *)

  let rec is_convertible size : vty * vty -> bool = function
    | Neu neu1, Neu neu2 -> is_convertible_neu size (neu1, neu2)
    | Univ l1, Univ l2 -> l1 = l2
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
  and is_convertible_neu size : neu * neu -> bool = function
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

  and synth ctx : Syntax.tm -> Semantics.vty =
    function
    | Let (_, def, body) ->
        let def_ty = synth ctx def in
        let def' = Semantics.eval ctx.tms def in
        synth (ctx |> bind_def def_ty def') body
    | Ann (expr, ty) ->
        let _ = is_ty ctx ty in
        let ty' = Semantics.eval ctx.tms ty in
        check ctx expr ty';
        ty'
    | Var x -> Env.get_index x ctx.tys
    | Univ L0 -> Univ L1
    (* There’s no type large enough to contain large universes in the core
       language, so raise an error here *)
    | Univ L1 -> failwith "cannot synthesize the type of large universes"
    | FunType (_, param_ty, body_ty) ->
        let l1 = is_ty ctx param_ty in
        let param_ty' = Semantics.eval ctx.tms param_ty in
        let l2 = is_ty (ctx |> bind_param param_ty') body_ty in
        Univ (Level.max l1 l2)
    | FunLit (name, param_ty, body) ->
        let _ = is_ty ctx param_ty in
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

  (** Check if the term is a type in a small or a large universe, and return the
      level of that universe. *)
  and is_ty ctx (tm : Syntax.tm) : Level.t =
    match synth ctx tm with
    | Univ l -> l
    | _ -> failwith "could not synthesise level: not a type"

end
