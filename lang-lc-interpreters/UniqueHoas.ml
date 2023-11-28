(** The lambda calculus, implemented using normalisation-by-evaluation with
    unique identifiers and higher-order abstract syntax (HOAS) in the semantic
    domain.
*)

module Id = Unique.Id


(** {1 Syntax} *)

type expr = Unique.expr =
  | Var of Id.t
  | Let of string * Id.t * expr * expr
  | FunLit of string * Id.t * expr
  | FunApp of expr * expr

(** {2 Conversions} *)

let of_named (e : Named.expr) : expr =
  Unique.of_named e

let to_named (e : expr) : Named.expr =
  Unique.to_named e

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  Unique.alpha_equiv e1 e2


(** {1 Semantics} *)

type value =
  | Neu of neu
  | FunLit of string * (value -> value)
and neu =
  | Var of Id.t
  | FunApp of neu * value

and clos = Id.t * env * expr

and env = value Id.Map.t

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var x -> Id.Map.find x vs
  | Let (_, i, def, body) -> eval (Id.Map.add i (eval vs def) vs) body
  | FunLit (x, i, body) -> FunLit (x, fun v -> eval (Id.Map.add i v vs) body)
  | FunApp (head, arg) -> begin
      match eval vs head with
      | FunLit (_, cl) -> cl (eval vs arg)
      | Neu nv -> Neu (FunApp (nv, eval vs arg))
  end

(** {2 Quotation} *)

let rec quote (v : value) : expr =
  match v with
  | Neu nv -> quote_neu nv
  | FunLit (x, cl) ->
      let i = Id.fresh () in
      FunLit (x, i, quote (cl (Neu (Var i))))
and quote_neu (nv : neu) : expr =
  match nv with
  | Var x -> Var x
  | FunApp (head, arg) -> FunApp (quote_neu head, quote arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (eval vs e)
