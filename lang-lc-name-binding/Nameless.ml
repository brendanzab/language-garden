(** The lambda calculus, implemented using de Bruijn indices, as demonstrated in
    “Types and Programming Languages”.

    This approach is expensive (requiring lots of tree-traversals and memory
    allocations) and error prone (it’s very easy to forget to shift a de Bruijn
    index), and I don’t recommend it for most implementations. See
    {!NamelessClosures} and {!NamelessHoas} for better approaches that employ
    de Bruijn indices.
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

(** {2 Shifting} *)

let shift (diff : int) (e : expr) : expr =
  let rec walk (cutoff : int) (e : expr) : expr =
    match e with
    | Var i -> if i >= cutoff then Var (i + diff) else Var i
    | Let (x, def, body) -> Let (x, walk cutoff def, walk (cutoff + 1) body)
    | FunLit (x, body) -> FunLit (x, walk (cutoff + 1) body)
    | FunApp (e1, e2) -> FunApp (walk cutoff e1, walk cutoff e2)
  in
  walk 0 e

(** {2 Substitution} *)

let subst (i, s : int * expr) (e : expr) : expr =
  let rec walk (cutoff : int) (e : expr) : expr =
    match e with
    | Var j -> if i = j + cutoff then shift cutoff s else e
    | Let (x, def, body) -> Let (x, walk cutoff def, walk (cutoff + 1) body)
    | FunLit (x, body) -> FunLit (x, walk (cutoff + 1) body)
    | FunApp (head, arg) -> FunApp (walk cutoff head, walk cutoff arg)
  in
  walk 0 e

let subst_top (s : expr) (e : expr) : expr =
  shift (-1) (subst (0, shift 1 s) e)


(** {1 Semantics} *)

let rec is_val (e : expr) : bool =
  match e with
  | FunLit _ -> true
  | _ -> false

(** {2 Evaluation} *)

let rec eval (e : expr) : expr =
  match e with
  | Var _ -> e
  | Let (_, def, body) ->
      let def = if is_val def then def else eval def in
      eval (subst_top def body)
  | FunLit (_, _) -> e
  | FunApp (head, arg) -> begin
      let head = if is_val head then head else eval head in
      let arg = if is_val arg then arg else eval arg in
      match head with
      | FunLit (_, body) -> eval (subst_top arg body)
      | head -> FunApp (head, arg)
  end

(** {2 Normalisation} *)

(** Fully normalise an expression, including under binders. *)
let rec normalise (e : expr) : expr =
  match e with
  | Var i -> Var i
  | Let (_, def, body) -> normalise (subst_top (normalise def) body)
  | FunLit (x, body) -> FunLit (x, normalise body)
  | FunApp (head, arg) -> begin
      match eval head with
      | FunLit (_, body) -> normalise (subst_top (normalise arg) body)
      | head -> FunApp (head, normalise arg)
  end
