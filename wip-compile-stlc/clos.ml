(** A version of the ANF language with explicit closures.

  The approach for closure types is based on the one found in “Compiling with
  Dependent Types” by William Bowman
*)

module Id = Symbol.Make ()

type id = Id.t
type name = Name.t

type ty =
  | Bool_ty                                                 (* Bool *)
  | Int_ty                                                  (* Int *)
  | Fun_ty of ty * ty                                       (* t1 -> t2 *)
  | Code_ty of ty * ty * ty                                 (* Code(t_env, t1, t2) *)
  | Tuple_ty of ty list                                     (* (t1, ... tn) *)

(** Complex expressions *)
type expr =
  | Let_comp of name * id * cexpr * expr                    (* let x : t := ce; e *)
  | Let_join of name * id * (name * id * ty) * expr * expr  (* let join j (x : t) : t := e1; e2 *)
  | Join_app of id * aexpr                                  (* jump j ae *)
  | Bool_elim of aexpr * expr * expr                        (* if ae then e1 else e2 *)
  | Comp of cexpr                                           (* ce *)

(** Computation expressions *)
and cexpr =
  | Fun_app of aexpr * aexpr                                (* ae1 ae2 *)
  | Tuple_proj of aexpr * int                               (* ae.n *)
  | Prim_app of Prim.t * aexpr list                         (* p ae1 ... aen *)
  | Atom of aexpr                                           (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                               (* x *)
  | Code_lit of (name * id * ty) * (name * id * ty) * expr  (* fun env x => e *)
  | Tuple_lit of aexpr list                                 (* (ae1, ..., aen) *)
  | Clos_lit of aexpr * aexpr                               (* clos(ae1, ae2) *)
  | Bool_lit of bool                                        (* true | false *)
  | Int_lit of int                                          (* ... | -1 | 0 | 1 | ... *)
