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
  | Bool_ty                                               (* Bool *)
  | Int_ty                                                (* Int *)
  | Fun_ty of ty * ty                                     (* t1 -> t2 *)
  | Tuple_ty of ty list                                   (* (t1, ... tn) *)

(** Computation expressions *)
and expr =
  | Let of name * id * ty * expr * expr                   (* let x : t := e; e *)
  | Fun_app of aexpr * aexpr                              (* ae1 ae2 *)
  | Tuple_proj of aexpr * int                             (* ae.n *)
  | Bool_elim of aexpr * expr * expr                      (* if ae then e1 else e2 *)
  | Atom of aexpr                                         (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                             (* x *)
  | Prim of Prim.t                                        (* #p *)
  | Fun_lit of name * id * ty * expr                      (* fun (x : t) => e *)
  | Tuple_lit of aexpr list                               (* (ae1, ..., aen) *)
  | Bool_lit of bool                                      (* true | false *)
  | Int_lit of int                                        (* ... | -1 | 0 | 1 | ... *)

module Semantics = struct

  type vexpr = Core.Semantics.vexpr =
    | Prim of Prim.t
    | Int_lit of int
    | Bool_lit of bool
    | Fun_lit of (vexpr -> vexpr)
    | Tuple_lit of vexpr list

  type env = (id * vexpr) list

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | Let(_, def_id, _, def, body) ->
        let def = eval env def in
        eval ((def_id, def) :: env) body
    | Fun_app (head, arg) ->
        begin match eval_atom env head with
        | Prim prim -> Core.Semantics.prim_app prim (eval_atom env arg)
        | Fun_lit body -> body (eval_atom env arg)
        | _ -> invalid_arg "expected function"
        end
    | Tuple_proj (head, label) ->
        begin match eval_atom env head with
        | Tuple_lit evs -> List.nth evs label
        | _ -> invalid_arg "expected tuple"
        end
    | Bool_elim (head, on_true, on_false) ->
        begin match eval_atom env head with
        | Bool_lit true -> eval env on_true
        | Bool_lit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end
    | Atom expr ->
        eval_atom env expr

  and eval_atom (env : env) (expr : aexpr) : vexpr =
    match expr with
    | Var name -> List.assoc name env
    | Prim prim -> Prim prim
    | Fun_lit (_, param_id, _, body) ->
        Fun_lit (fun arg -> eval ((param_id, arg) :: env) body)
    | Tuple_lit exprs ->
        Tuple_lit (List.map (eval_atom env) exprs)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

end
