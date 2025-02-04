(** The lambda calculus, implemented using normalisation-by-evaluation with
    names and defunctionalised, first-order closures in the semantic domain.
*)

(** {1 Syntax} *)

type expr = Named.expr =
  | Var of string
  | Let of string * expr * expr
  | Fun_lit of string * expr
  | Fun_app of expr * expr

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  Named.alpha_equiv e1 e2


(** {1 Semantics} *)

type value =
  | Neu of neu
  | Fun_lit of clos
and neu =
  | Var of string
  | Fun_app of neu * value

and clos = string * env * expr
and env = (string * value) list

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var x -> List.assoc x vs
  | Let (x, def, body) -> eval ((x, eval vs def) :: vs) body
  | Fun_lit (x, body) -> Fun_lit (x, vs, body)
  | Fun_app (head, arg) -> fun_app (eval vs head) (eval vs arg)

and fun_app (head : value) (arg : value) =
  match head with
  | Fun_lit body -> clos_app body arg
  | Neu nv -> Neu (Fun_app (nv, arg))

and clos_app (x, vs, e : clos) (arg : value) : value =
  eval ((x, arg) :: vs) e

(** {2 Quotation} *)

let rec fresh (ns : string list) (x : string) : string =
  match List.mem x ns with
  | true -> fresh ns (x ^ "'")
  | false -> x

let rec quote (ns : string list) (v : value) : expr =
  match v with
  | Neu nv -> quote_neu ns nv
  | Fun_lit (x, _, _ as body) ->
      let x = fresh ns x in
      Fun_lit (x, quote (x :: ns) (clos_app body (Neu (Var x))))
and quote_neu (ns : string list) (nv : neu) : expr =
  match nv with
  | Var x -> Var x
  | Fun_app (head, arg) -> Fun_app (quote_neu ns head, quote ns arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (List.map fst vs) (eval vs e)
