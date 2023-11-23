(** The lambda calculus, implemented using locally nameless terms. *)

type expr =
  | BoundVar of string
  | FreeVar of int (* de Bruijn index *)
  | Let of expr * expr
  | FunLit of expr
  | FunApp of expr * expr

(* TODO *)
