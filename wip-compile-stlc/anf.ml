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
  | Bool_ty                                                 (* Bool *)
  | Int_ty                                                  (* Int *)
  | Fun_ty of ty * ty                                       (* t1 -> t2 *)
  | Tuple_ty of ty list                                     (* (t1, ... tn) *)

(** Complex expressions *)
type expr =
  | Let_comp of name * id * cexpr * expr                    (* let x : t := ce; e *)
  | Let_join of name * id * (name * id * ty) * expr * expr  (* letjoin j (x : t) : t := e1; e2 *)
  | Join_app of id * aexpr                                  (* jump j ae *)
  | Bool_elim of aexpr * expr * expr                        (* if ae then e1 else e2 *)
  | Comp of cexpr                                           (* ce *)

(** Computation expressions

    These are anything that can be bound by a let expression or returned at
    the end of a complex expression. The original paper by Flanagan, et. al.
    flattened these into complex expressions, but this is an equivalent
    approach.
*)
and cexpr =
  | Fun_app of aexpr * aexpr                                (* ae1 ae2 *)
  | Tuple_proj of aexpr * int                               (* ae.n *)
  | Atom of aexpr                                           (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                               (* x *)
  | Prim of Prim.t                                          (* #p *)
  | Fun_lit of name * id * ty * expr                        (* fun (x : t) => e *)
  | Tuple_lit of aexpr list                                 (* (ae1, ..., aen) *)
  | Bool_lit of bool                                        (* true | false *)
  | Int_lit of int                                          (* ... | -1 | 0 | 1 | ... *)

module Semantics = struct

  type vexpr = Core.Semantics.vexpr =
    | Prim of Prim.t
    | Int_lit of int
    | Bool_lit of bool
    | Fun_lit of (vexpr -> vexpr)
    | Tuple_lit of vexpr list

  type defn =
    | Value of vexpr
    | Join of id * expr

  type env = (id * defn) list

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | Let_comp (_, def_id, def, body) ->
        let def = eval_comp env def in
        eval ((def_id, Value def) :: env) body
    | Let_join (_, def_id, (_, param_id, _), def, body) ->
        let def = Join (param_id, def) in
        eval ((def_id, def) :: env) body
    | Join_app (id, arg) ->
        begin match List.assoc id env with
        | Join (param_id, body) ->
            let arg = eval_atom env arg in
            eval ((param_id, Value arg) :: env) body
        | _ -> invalid_arg "expected join"
        end
    | Bool_elim (head, on_true, on_false) ->
        begin match eval_atom env head with
        | Bool_lit true -> eval env on_true
        | Bool_lit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end
    | Comp expr ->
        eval_comp env expr

  and eval_comp (env : env) (expr : cexpr) : vexpr =
    match expr with
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
    | Atom expr ->
        eval_atom env expr

  and eval_atom (env : env) (expr : aexpr) : vexpr =
    match expr with
    | Var name ->
        begin match List.assoc name env with
        | Value expr -> expr
        | _ -> invalid_arg "expected value"
        end
    | Prim prim -> Prim prim
    | Fun_lit (_, param_id, _, body) ->
        Fun_lit (fun arg -> eval ((param_id, Value arg) :: env) body)
    | Tuple_lit exprs ->
        Tuple_lit (List.map (eval_atom env) exprs)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

end
