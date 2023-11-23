(** The lambda calculus, implemented using normalisation-by-evaluation with de
    Bruijn indices in the syntax, and levels and higher-order abstract syntax
    (HOAS) in the semantic domain.
*)

(** {1 Syntax} *)

type expr =
  | Var of int (* de Bruijn index *)
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr


(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let rec alpha_equiv (e1 : expr) (e2 : expr) =
  match e1, e2 with
  | Var i1, Var i2 -> i1 = i2
  | Let (_, def1, body1), Let (_, def2, body2) ->
      alpha_equiv def1 def2 && alpha_equiv body1 body2
  | FunLit (_, body1), FunLit (_, body2) ->
      alpha_equiv body1 body2
  | FunApp (head1, arg1), FunApp (head2, arg2) ->
      alpha_equiv head1 head2 && alpha_equiv arg1 arg2
  | _, _ -> false


(** {1 Semantics} *)

type value =
  | Neu of neu
  | FunLit of string * (value -> value)
and neu =
  | Var of int (* de Bruijn level *)
  | FunApp of neu * value

type env = value list

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var i -> List.nth vs i
  | Let (_, def, body) -> eval (eval vs def :: vs) body
  | FunLit (x, body) -> FunLit (x, fun v -> eval (v :: vs) body)
  | FunApp (head, arg) -> begin
      match eval vs head with
      | FunLit (_, body) -> body (eval vs arg)
      | Neu nv -> Neu (FunApp (nv, eval vs arg))
  end

(** {2 Quotation} *)

let rec quote (size : int) (v : value) : expr =
  match v with
  | Neu nv -> quote_neu size nv
  | FunLit (x, body) -> FunLit (x, quote (size + 1) (body (Neu (Var size))))
and quote_neu (size : int) (nv : neu) : expr =
  match nv with
  | Var l -> Var (size - l - 1)
  | FunApp (head, arg) -> FunApp (quote_neu size head, quote size arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (List.length vs) (eval vs e)
