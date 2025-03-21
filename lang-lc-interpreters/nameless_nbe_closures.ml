(** The lambda calculus, implemented using normalisation-by-evaluation with de
    Bruijn indices in the syntax, and levels with defunctionalised, first-order
    closures in the semantic domain.
*)

(** {1 Syntax} *)

(** De Bruijn index *)
type index = int

type expr = Nameless.expr =
  | Var of index
  | Let of string * expr * expr
  | Fun_lit of string * expr
  | Fun_app of expr * expr

(** {2 Conversions} *)

let of_named (e : Named.expr) : expr =
  Nameless.of_named e

let to_named (e : expr) : Named.expr =
  Nameless.to_named e

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  Nameless.alpha_equiv e1 e2


(** {1 Semantics} *)

(** De Bruijn level *)
type level = int

type value =
  | Neu of neu
  | Fun_lit of string * clos
and neu =
  | Var of level
  | Fun_app of neu * value

and clos = env * expr
and env = value list

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var i -> List.nth vs i
  | Let (_, def, body) -> eval (eval vs def :: vs) body
  | Fun_lit (x, body) -> Fun_lit (x, (vs, body))
  | Fun_app (head, arg) -> fun_app (eval vs head) (eval vs arg)

and fun_app (head : value) (arg : value) =
  match head with
  | Fun_lit (_, body) -> clos_app body arg
  | Neu nv -> Neu (Fun_app (nv, arg))

and clos_app (vs, e : clos) (arg : value) : value =
  eval (arg :: vs) e

(** {2 Quotation} *)

let rec quote (size : int) (v : value) : expr =
  match v with
  | Neu nv -> quote_neu size nv
  | Fun_lit (x, body) -> Fun_lit (x, quote (size + 1) (clos_app body (Neu (Var size))))
and quote_neu (size : int) (nv : neu) : expr =
  match nv with
  | Var l -> Var (size - l - 1)
  | Fun_app (head, arg) -> Fun_app (quote_neu size head, quote size arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (List.length vs) (eval vs e)
