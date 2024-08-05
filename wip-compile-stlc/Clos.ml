(** A version of the ANF language with explicit closures.

  The approach for closure types is based on the one found in “Compiling with
  Dependent Types” by William Bowman
*)

module Id = Symbol.Make ()

type id = Id.t
type name = Name.t

type ty =
  | BoolTy                                                (* Bool *)
  | IntTy                                                 (* Int *)
  | FunTy of ty * ty                                      (* t1 -> t2 *)
  | CodeTy of ty * ty * ty                                (* Code(t_env, t1, t2) *)
  | TupleTy of ty list                                    (* (t1, ... tn) *)

(** Complex expressions *)
type expr =
  | LetComp of name * id * cexpr * expr                   (* let x : t := ce; e *)
  | LetJoin of name * id * (name * id * ty) * expr * expr (* let join j (x : t) : t := e1; e2 *)
  | JoinApp of id * aexpr                                 (* jump j ae *)
  | BoolElim of aexpr * expr * expr                       (* if ae then e1 else e2 *)
  | Comp of cexpr                                         (* ce *)

(** Computation expressions *)
and cexpr =
  | PrimApp of Prim.t * aexpr list                        (* p ae1 ... aen *)
  | FunApp of aexpr * aexpr                               (* ae1 ae2 *)
  | TupleProj of aexpr * int                              (* ae.n *)
  | Atom of aexpr                                         (* ae *)

(** Atomic expressions *)
and aexpr =
  | Var of id                                             (* x *)
  | CodeLit of (name * id * ty) * (name * id * ty) * expr (* fun env x => e *)
  | TupleLit of aexpr list                                (* (ae1, ..., aen) *)
  | ClosLit of aexpr * aexpr                              (* clos(ae1, ae2) *)
  | BoolLit of bool                                       (* true | false *)
  | IntLit of int                                         (* ... | -1 | 0 | 1 | ... *)
