(** A language that enforces a fixed evaluation order by binding every
    intermediate computation to a corresponding local binding.

    - {{:https://en.wikipedia.org/wiki/A-normal_form}
      A-Normal Form} on Wikipedia
    - {{:https://doi.org/10.1145/173262.155113}
      The essence of compiling with continuations} by Flanagan, et. al.
    - {{:https://matt.might.net/articles/a-normalization/}
      A-Normalization: Why and How (with code)} by Matt Might
*)

module Id = Symbol.Make ()

type id = Id.t
type name = Name.t

type ty = Core.ty =
  | BoolTy                                                (* Bool *)
  | IntTy                                                 (* Int *)
  | FunTy of ty * ty                                      (* t1 -> t2 *)
  | TupleTy of ty list                                    (* (t1, ... tn) *)

(** Complex expressions *)
type expr =
  | LetComp of name * id * cexpr * expr                   (* let x : t := ce; e *)
  | LetJoin of name * id * (name * id * ty) * expr * expr (* letjoin j (x : t) : t := e1; e2 *)
  | JoinApp of id * aexpr                                 (* jump j ae *)
  | BoolElim of aexpr * expr * expr                       (* if ae then e1 else e2 *)
  | Comp of cexpr                                         (* ce *)

(** Computation expressions

    These are anything that can be bound by a let expression or returned at
    the end of a complex expression. The original paper by Flanagan, et. al.
    flattened these into complex expressions, but this is an equivalent
    approach.
*)
and cexpr =
  | FunApp of aexpr * aexpr                               (* ae1 ae2 *)
  | TupleProj of aexpr * int                              (* ae.n *)
  | PrimApp of Prim.t * aexpr list                        (* p ae1 ... aen *)
  | Atom of aexpr                                         (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                             (* x *)
  | FunLit of name * id * ty * expr                       (* fun (x : t) => e *)
  | TupleLit of aexpr list                                (* (ae1, ..., aen) *)
  | BoolLit of bool                                       (* true | false *)
  | IntLit of int                                         (* ... | -1 | 0 | 1 | ... *)

module Semantics = struct

  type vexpr = Core.Semantics.vexpr =
    | IntLit of int
    | BoolLit of bool
    | FunLit of (vexpr -> vexpr)
    | TupleLit of vexpr list

  type defn =
    | Value of vexpr
    | Join of id * expr

  type env = (id * defn) list

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | LetComp (_, def_id, def, body) ->
        let def = eval_comp env def in
        eval ((def_id, Value def) :: env) body
    | LetJoin (_, def_id, (_, param_id, _), def, body) ->
        let def = Join (param_id, def) in
        eval ((def_id, def) :: env) body
    | JoinApp (id, arg) ->
        begin match List.assoc id env with
        | Join (param_id, body) ->
            let arg = eval_atom env arg in
            eval ((param_id, Value arg) :: env) body
        | _ -> invalid_arg "expected join"
        end
    | BoolElim (head, on_true, on_false) ->
        begin match eval_atom env head with
        | BoolLit true -> eval env on_true
        | BoolLit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end
    | Comp expr ->
        eval_comp env expr

  and eval_comp (env : env) (expr : cexpr) : vexpr =
    match expr with
    | FunApp (head, arg) ->
        begin match eval_atom env head with
        | FunLit body -> body (eval_atom env arg)
        | _ -> invalid_arg "expected function"
        end
    | TupleProj (head, label) ->
        begin match eval_atom env head with
        | TupleLit evs -> List.nth evs label
        | _ -> invalid_arg "expected tuple"
        end
    | PrimApp (prim, args) ->
        Core.Semantics.eval_prim prim (List.map (eval_atom env) args)
    | Atom expr ->
        eval_atom env expr

  and eval_atom (env : env) (expr : aexpr) : vexpr =
    match expr with
    | Var name ->
        begin match List.assoc name env with
        | Value expr -> expr
        | _ -> invalid_arg "expected value"
        end
    | FunLit (_, param_id, _, body) ->
        FunLit (fun arg -> eval ((param_id, Value arg) :: env) body)
    | TupleLit exprs ->
        TupleLit (List.map (eval_atom env) exprs)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

end
