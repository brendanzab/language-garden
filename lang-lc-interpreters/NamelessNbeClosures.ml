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
  | FunLit of string * expr
  | FunApp of expr * expr

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
  | FunLit of string * clos
and neu =
  | Var of level
  | FunApp of neu * value

and clos = env * expr

and env = value list

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var i -> List.nth vs i
  | Let (_, def, body) -> eval (eval vs def :: vs) body
  | FunLit (x, body) -> FunLit (x, (vs, body))
  | FunApp (head, arg) -> begin
      match eval vs head with
      | FunLit (_, body) -> inst body (eval vs arg)
      | Neu nv -> Neu (FunApp (nv, eval vs arg))
  end
and inst (vs, e : clos) (arg : value) : value =
  eval (arg :: vs) e

(** {2 Quotation} *)

let rec quote (size : int) (v : value) : expr =
  match v with
  | Neu nv -> quote_neu size nv
  | FunLit (x, body) -> FunLit (x, quote (size + 1) (inst body (Neu (Var size))))
and quote_neu (size : int) (nv : neu) : expr =
  match nv with
  | Var l -> Var (size - l - 1)
  | FunApp (head, arg) -> FunApp (quote_neu size head, quote size arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (List.length vs) (eval vs e)
