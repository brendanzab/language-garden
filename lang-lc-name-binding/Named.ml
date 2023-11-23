(** The lambda calculus, implemented using names. *)

(** {1 Syntax} *)

type expr =
  | Var of string
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  (** Compare for alpha equivalence by comparing the binding depth of variable
      names in each expression. This approach is described in section 6.1 of
      {{:https://davidchristiansen.dk/tutorials/implementing-types-hs.pdf}
      “Checking Dependent Types with Normalization by Evaluation: A Tutorial
      (Haskell Version)”} by David Christiansen *)
  let rec go (size : int) (ns1, e1 : (string * int) list * expr) (ns2, e2 : (string * int) list * expr) : bool =
    match e1, e2 with
    | Var x1, Var x2 -> begin
        match List.assoc_opt x1 ns1, List.assoc_opt x2 ns2 with
        | None, None -> x1 = x2
        | Some l1, Some l2  -> l1 = l2
        | _, _ -> false
    end
    | Let (x1, def1, body1), Let (x2, def2, body2) ->
        go size (ns1, def1) (ns2, def2)
          && go (size + 1) ((x1, size) :: ns1, body1) ((x2, size) :: ns2, body2)
    | FunLit (x1, body1), FunLit (x2, body2) ->
      go (size + 1) ((x1, size) :: ns1, body1) ((x2, size) :: ns2, body2)
    | FunApp (head1, arg1), FunApp (head2, arg2) ->
        go size (ns1, head1) (ns2, head2) && go size (ns1, arg1) (ns2, arg2)
    | _, _ -> false
  in
  go 0 ([], e1) ([], e2)

(** {2 Substitution} *)

let rec subst (x, s : string * expr) (e : expr) : expr =
  match e with
  | Var y -> if x = y then s else e
  | Let (y, def, body) -> if x = y then e else Let (y, def, subst (x, s) body)
  | FunLit (y, body) -> if x = y then e else FunLit (y, subst (x, s) body)
  | FunApp (head, arg) -> FunApp (subst (x, s) head, subst (x, s) arg)


(** {1 Semantics} *)

let rec is_val (e : expr) : bool =
  match e with
  | FunLit _ -> true
  | _ -> false

(** {2 Evaluation} *)

exception NoRuleApplies

(** Small-step evaluation *)
let rec eval1 (e : expr) : expr =
  match e with
  | Let (x, def, body) when is_val def -> subst (x, def) body
  | Let (x, def, body) -> Let (x, eval1 def, body)
  | FunApp (FunLit (x, body), arg) when is_val arg -> subst (x, arg) body
  | FunApp (head, arg) when is_val head -> FunApp (head, eval1 arg)
  | FunApp (head, arg) -> FunApp (eval1 head, arg)
  | Var _ | FunLit (_, _) -> raise NoRuleApplies

(** Evaluate an expression by repeatedly applying the small-step evaluation
    rules until no more apply.  *)
let rec eval (e : expr) : expr =
  try
    let e' = eval1 e in
    (eval [@tailcall]) e'
  with
    | NoRuleApplies -> e

(** {2 Normalisation} *)

(** Fully normalise an expression, including under binders. *)
let rec normalise (e : expr) : expr =
  match e with
  | Var x -> Var x
  | Let (x, def, body) -> normalise (subst (x, normalise def) body)
  | FunLit (x, body) -> FunLit (x, normalise body)
  | FunApp (head, arg) -> begin
      match normalise head with
      | FunLit (x, body) -> normalise (subst (x, normalise arg) body)
      | head -> FunApp (head, normalise arg)
  end
