(** Monadic form, which separates values from effectful computations.

    - “A New One-Pass Transformation into Monadic Normal Form” by Olivier Danvy
      https://doi.org/10.1007/3-540-36579-6_6
    - “A low-level look at A-normal form”, Section 2.2 by William Bowman
      https://www.williamjbowman.com/tmp/wjb2024-anf-is-dead.pdf
*)

module Id = Symbol.Make ()

type id = Id.t
type name = Name.t

type ty = Core.ty =
  | BoolTy                                                (* Bool *)
  | IntTy                                                 (* Int *)
  | FunTy of ty * ty                                      (* t1 -> t2 *)
  | TupleTy of ty list                                    (* (t1, ... tn) *)

(** Computation expressions *)
and expr =
  | Let of name * id * ty * expr * expr                   (* let x : t := e; e *)
  | FunApp of aexpr * aexpr                               (* ae1 ae2 *)
  | TupleProj of aexpr * int                              (* ae.n *)
  | BoolElim of aexpr * expr * expr                       (* if ae then e1 else e2 *)
  | Atom of aexpr                                         (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                             (* x *)
  | Prim of Prim.t                                        (* #p *)
  | FunLit of name * id * ty * expr                       (* fun (x : t) => e *)
  | TupleLit of aexpr list                                (* (ae1, ..., aen) *)
  | BoolLit of bool                                       (* true | false *)
  | IntLit of int                                         (* ... | -1 | 0 | 1 | ... *)

module Semantics = struct

  type vexpr = Core.Semantics.vexpr =
    | Prim of Prim.t
    | IntLit of int
    | BoolLit of bool
    | FunLit of (vexpr -> vexpr)
    | TupleLit of vexpr list

  type env = (id * vexpr) list

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | Let(_, def_id, _, def, body) ->
        let def = eval env def in
        eval ((def_id, def) :: env) body
    | FunApp (head, arg) ->
        begin match eval_atom env head with
        | Prim prim -> Core.Semantics.prim_app prim (eval_atom env arg)
        | FunLit body -> body (eval_atom env arg)
        | _ -> invalid_arg "expected function"
        end
    | TupleProj (head, label) ->
        begin match eval_atom env head with
        | TupleLit evs -> List.nth evs label
        | _ -> invalid_arg "expected tuple"
        end
    | BoolElim (head, on_true, on_false) ->
        begin match eval_atom env head with
        | BoolLit true -> eval env on_true
        | BoolLit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end
    | Atom expr ->
        eval_atom env expr

  and eval_atom (env : env) (expr : aexpr) : vexpr =
    match expr with
    | Var name -> List.assoc name env
    | Prim prim -> Prim prim
    | FunLit (_, param_id, _, body) ->
        FunLit (fun arg -> eval ((param_id, arg) :: env) body)
    | TupleLit exprs ->
        TupleLit (List.map (eval_atom env) exprs)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

end
