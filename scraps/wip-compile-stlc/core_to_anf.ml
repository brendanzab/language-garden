(** A translation that normalises the core language into A-normal form, making
    the evaluation order explicit. *)

module Src_env : sig

  type t

  val empty : t
  val extend : Anf.id -> Core.ty -> t -> t

  val lookup_id : int -> t -> Anf.id
  val type_of : Core.expr -> t -> Anf.ty

end = struct

  type t = {
    ids : Anf.id list;    (* mapping from source variables to target ids *)
    tys : Core.ty list;   (* mapping from source variables to source types *)
  }

  let empty : t = {
    ids = [];
    tys = [];
  }

  let extend (id : Anf.id) (ty : Core.ty) (env : t) = {
    ids = id :: env.ids;
    tys = ty :: env.tys;
  }

  let lookup_id (index : Core.index) (env : t) : Anf.id =
    List.nth env.ids index

  let type_of (expr : Core.expr) (env : t) : Anf.ty =
    Core.type_of env.tys expr

end

type 'a cont = Src_env.t -> 'a -> Anf.expr

let comp : Anf.cexpr cont =
  fun _ expr -> Comp expr

let join_app (id : Anf.id) : Anf.aexpr cont =
  fun _ expr -> Join_app (id, expr)

(* This could probably be implemented more cleanly with a continuation and
  reader monad transformer stack, but I wanted to keep things more explicit
  for now. *)

let rec translate (expr : Core.expr) : Anf.cexpr cont cont =
  fun env k ->
    match expr with
    | Var index ->
        k env (Atom (Var (Src_env.lookup_id index env)))
    | Prim prim ->
        k env (Atom (Anf.Prim prim))
    | Let (def_name, def_ty, def, body) ->
        translate def env @@ fun env def ->
          let def_id = Anf.Id.fresh () in
          let body = translate body (Src_env.extend def_id def_ty env) k in
          Let_comp (def_name, def_id, def, body)
    | Fun_lit (param_name, param_ty, body) ->
        let param_id = Anf.Id.fresh () in
        let body = translate body (Src_env.extend param_id param_ty env) comp in
        k env (Atom (Fun_lit (param_name, param_id, param_ty, body)))
    | Fun_app (head, arg) ->
        translate_name "head" head env @@ fun env head ->
          translate_name "arg" arg env @@ fun env arg ->
            k env (Fun_app (head, arg))
    | Tuple_lit args ->
        translate_names "elem" args env @@ fun env args ->
          k env (Atom (Tuple_lit args))
    | Tuple_proj (head, label) ->
        translate_name "head" head env @@ fun env head ->
          k env (Tuple_proj (head, label))
    | Bool_lit b ->
        k env (Atom (Bool_lit b))
    | Bool_elim (head, on_true, on_false) ->
        translate_name "head" head env @@ fun env head ->
          let def_id = Anf.Id.fresh () in
          let param_id = Anf.Id.fresh () in
          let expr_ty = Src_env.type_of expr env in
          Let_join (
            Machine "cont",
            def_id,
            (Machine "param", param_id, expr_ty),
            k env (Atom (Var param_id)),
            Bool_elim (head,
              translate_name "on-true" on_true env (join_app def_id),
              translate_name "on-false" on_false env (join_app def_id)))
    | Int_lit i ->
        k env (Atom (Int_lit i))

(** Translate an expression to ANF, binding it to an intermediate definition
    if needed. *)
and translate_name (name : string) (expr : Core.expr) : Anf.aexpr cont cont =
  fun env k ->
    translate expr env @@ fun env expr ->
      match expr with
      | Anf.Atom expr -> k env expr
      | expr ->
          let def_id = Anf.Id.fresh () in
          Let_comp (Machine name, def_id, expr, k env (Var def_id))

(** Translate a sequence of expressions, binding them to intermediate
    definitions if needed. *)
and translate_names (name : string) (exprs : Core.expr list) : Anf.aexpr list cont cont =
  fun env k ->
    match exprs with
    | [] -> k env []
    | expr :: exprs ->
        translate_name name expr env @@ fun env expr ->
          translate_names name exprs env @@ fun env exprs ->
            k env (expr :: exprs)

let translate (expr : Core.expr) : Anf.expr =
  translate expr Src_env.empty comp
