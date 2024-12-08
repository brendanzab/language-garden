module Ns = Core.Ns
module Syntax = Core.Syntax
module Semantics = Core.Semantics


exception Error of string


type var = {
  ty : Semantics.vty;
  level : Ns.tm Env.level;
}


module Context = struct

  type t = {
    size : Ns.tm Env.size;
    names : (Ns.tm, Core.name) Env.t;
    tys : (Ns.tm, Semantics.vty) Env.t;
    tms : (Ns.tm, Semantics.vtm) Env.t;
  }


  let empty = {
    size = Env.empty_size;
    names = Env.empty;
    tys = Env.empty;
    tms = Env.empty;
  }

  let extend (ctx : t) (name : Core.name) (ty : Semantics.vty) (tm : Semantics.vtm) = {
    size = ctx.size |> Env.bind_level;
    names = ctx.names |> Env.bind_entry name;
    tys = ctx.tys |> Env.bind_entry ty;
    tms = ctx.tms |> Env.bind_entry tm;
  }

  let lookup (ctx : t) (name : Core.name) : (Semantics.vty * Ns.tm Env.index) option =
    Env.entry_index name ctx.names
      |> Option.map (fun x -> ctx.tys |> Env.lookup x, x)


  (** Run a continuation with a definition added to the context *)
  let define (type a) (ctx : t) (name : Core.name) (ty : Semantics.vty) (tm : Semantics.vtm) (body : t -> var -> a) : a =
    let level = Env.next_level ctx.size in
    body (extend ctx name ty tm) { ty; level }

  (** Run a continuation with an assumption added to the context *)
  let assume (type a) (ctx : t) (name : Core.name) (ty : Semantics.vty) (body : t -> var -> a) : a =
    let level = Env.next_level ctx.size in
    body (extend ctx name ty (Neu (Var level))) { ty; level }


  let eval (ctx : t) (tm : Syntax.tm) : Semantics.vtm =
    Semantics.eval ctx.tms tm

  let quote (ctx : t) (vtm : Semantics.vtm) : Syntax.tm =
    Semantics.quote ctx.size vtm

  let is_convertible (ctx : t) (v0 : Semantics.vtm) (v1 : Semantics.vtm) : bool =
    Semantics.is_convertible ctx.size (v0, v1)

end


type is_ty =
  Context.t -> Core.Level.t * Syntax.tm

type synth =
  Context.t -> Semantics.vty * Syntax.tm

type check =
  Context.t -> Semantics.vty -> Syntax.tm


let run_is_ty (ty : is_ty) : Core.Level.t * Syntax.tm =
  ty Context.empty

let run_check (expected_ty : Semantics.vty) (tm : check) : Syntax.tm =
  tm Context.empty expected_ty

let run_synth (tm : synth) : Semantics.vty * Syntax.tm =
  tm Context.empty


let var (x : var) : synth =
  fun ctx ->
    x.ty, Var (Env.level_to_index ctx.size x.level)

let is_ty (tm : synth) : is_ty =
  fun ctx ->
    match tm ctx with
    | Univ l1, tm -> l1, tm
    | _, _ -> raise (Error "not a type")

let check (tm : synth) : check =
  fun ctx expected_ty ->
    let (ty, tm) = tm ctx in
    if Context.is_convertible ctx ty expected_ty then tm else
      raise (Error "type mismatch")

let ann ~(ty : is_ty) (tm : check) : synth =
  fun ctx ->
    let _, ty = ty ctx in
    let ty = Context.eval ctx ty in
    ty, tm ctx ty


let eval (tm : Syntax.tm) : Semantics.vtm =
  Semantics.eval Env.empty tm

let quote (vtm : Semantics.vtm) : Syntax.tm =
  Semantics.quote Env.empty_size vtm

let is_convertible (v0 : Semantics.vtm) (v1 : Semantics.vtm) : bool =
  Semantics.is_convertible Env.empty_size (v0, v1)


module Structure = struct

  let name (x : string) : synth =
    fun ctx ->
      match Context.lookup ctx (Some x) with
      | Some (ty, x) -> ty, Syntax.Var x
      | None -> raise (Error ("'" ^ x ^ "' was not bound in scope"))

  let let_synth ?name (def : synth) (body : synth -> synth) : synth =
    fun ctx ->
      let def_ty, def = def ctx in
      Context.define ctx name def_ty (Context.eval ctx def)
        (fun ctx x ->
          let body_ty, body = body (var x) ctx in
          body_ty, Syntax.Let (name, def, body))

  let let_check ?name (def : synth) (body : synth -> check) : check =
    fun ctx body_ty ->
      let def_ty, def = def ctx in
      Context.define ctx name def_ty (Context.eval ctx def)
        (fun ctx x ->
          Syntax.Let (name, def, body (var x) ctx body_ty))

end


module Fun = struct

  let intro_synth ?name ~ty:param_ty (body : synth -> synth) : synth =
    fun ctx ->
      let _, param_ty = param_ty ctx in
      let body_ty, body =
        Context.assume ctx name (Context.eval ctx param_ty)
          (fun ctx x ->
            let body_ty, body = body (var x) ctx in
            Context.quote ctx body_ty, body)
      in
      Context.eval ctx (Syntax.FunType (name, param_ty, body_ty)),
      Syntax.FunLit (name, param_ty, body)

  let check_param_ty (ty : is_ty option) : check =
    fun ctx expected_ty ->
      match ty with
      | Some ty ->
          let _, ty = ty ctx in
          if Context.(is_convertible ctx (eval ctx ty) expected_ty) then ty else
            raise (Error "mismatched parameter type")
      | None -> Context.quote ctx expected_ty

  let intro_check ?name ?ty:param_ty (body : synth -> check) : check =
    fun ctx expected_ty ->
      match expected_ty with
      | Semantics.FunType (_, expected_param_ty, body_ty) ->
          let expected_param_ty = Lazy.force expected_param_ty in
          let param_ty = check_param_ty param_ty ctx expected_param_ty in
          Context.assume ctx name expected_param_ty
            (fun ctx x ->
              let x = var x in
              let body_ty = body_ty (Context.eval ctx (x ctx |> snd)) in
              Syntax.FunLit (name, param_ty, body x ctx body_ty))
      | _ -> raise (Error "not a function type")

  let app (head : synth) (arg : check) : synth =
    fun ctx ->
      match head ctx with
      | Semantics.FunType (_, param_ty, body_ty), head ->
          let arg = arg ctx (Lazy.force param_ty) in
          let body_ty = body_ty (Context.eval ctx arg) in
          body_ty, Syntax.FunApp (head, arg)
      | _, _ -> raise (Error "expected a function type")

end


module Univ = struct

  let univ (l : Core.Level.t) : synth =
    fun _ ->
      match l with
      | L0 -> Univ L1, Univ L0
      | L1 -> raise (Error "Type 1 has no type")

  let fun_ ?name (param_ty : synth) (body_ty : synth -> synth) : synth =
    fun ctx ->
      let l1, param_ty = is_ty param_ty ctx in
      Context.assume ctx name (Context.eval ctx param_ty)
        (fun ctx x ->
          let l2, body_ty = is_ty (body_ty (var x)) ctx in
          Semantics.Univ (Core.Level.max l1 l2),
          Syntax.FunType (name, param_ty, body_ty))

end
