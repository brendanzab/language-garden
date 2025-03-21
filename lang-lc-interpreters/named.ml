(** The lambda calculus, implemented using names.

    This is one of the most naive approaches you could use to implement the
    lambda calculus. We just use the names from the original lambda terms
    directly, meaning that in order to implement capture avoiding substitution
    we need to compute free variable sets, using those to freshen variables
    appropriately. Alpha equivalence also needs to take into account of names,
    building up substitutions from names to levels.
*)

(** {1 Syntax} *)

type expr =
  | Var of string
  | Let of string * expr * expr
  | Fun_lit of string * expr
  | Fun_app of expr * expr

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  (* Compare for alpha equivalence by comparing the binding depth of variable
    names in each expression. This approach is described in section 6.1 of
    {{: https://davidchristiansen.dk/tutorials/implementing-types-hs.pdf}
    “Checking Dependent Types with Normalization by Evaluation: A Tutorial
    (Haskell Version)”} by David Christiansen. *)
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
    | Fun_lit (x1, body1), Fun_lit (x2, body2) ->
        go (size + 1) ((x1, size) :: ns1, body1) ((x2, size) :: ns2, body2)
    | Fun_app (head1, arg1), Fun_app (head2, arg2) ->
        go size (ns1, head1) (ns2, head2) && go size (ns1, arg1) (ns2, arg2)
    | _, _ -> false
  in
  go 0 ([], e1) ([], e2)

(** {2 Variable sets} *)

module String_set = Set.Make (String)

let rec fresh (ns : String_set.t) (x : string) : string =
  match String_set.mem x ns with
  | true -> fresh ns (x ^ "'")
  | false -> x

let rec free_vars (e : expr) : String_set.t =
  match e with
  | Var x -> String_set.singleton x
  | Let (x, def, body) -> String_set.(union (free_vars def) (remove x (free_vars body)))
  | Fun_lit (x, body) -> String_set.remove x (free_vars body)
  | Fun_app (head, arg) -> String_set.union (free_vars head) (free_vars arg)

let rec all_vars (e : expr) : String_set.t =
  match e with
  | Var x -> String_set.singleton x
  | Let (_, def, body) -> String_set.union (all_vars def) (all_vars body)
  | Fun_lit (_, body) -> all_vars body
  | Fun_app (head, arg) -> String_set.union (all_vars head) (all_vars arg)

(** {2 Substitution} *)

(** Capture avoiding substitution *)
let rec subst (x, s : string * expr) (e : expr) : expr =
  (* Based on https://github.com/sweirich/lambda-n-ways/blob/c4ffc2add78d63b89c3d5d872f6787dadb890626/lib/Named/Lennart.hs#L83-L114 *)
  let fvs = free_vars s in
  let vs = String_set.add x fvs in
  let freshen_body y body =
    (* NOTE: Just the free variables are sufficient, but it’s faster to collect
       all of the variables without removing the bound ones. *)
    let y' = fresh (String_set.union vs (all_vars body)) y in
    y', subst (y, Var y') body
  in
  let rec go e =
    match e with
    | Var y -> if x = y then s else e
    | Let (y, _, _) when x = y -> e
    | Let (y, def, body) when String_set.mem y fvs ->
        let y', body' = freshen_body y body in
        Let (y', go def, go body')
    | Let (y, def, body) -> Let (y, def, go body)
    | Fun_lit (y, _) when x = y -> e
    | Fun_lit (y, body) when String_set.mem y fvs ->
        let y', body' = freshen_body y body in
        Fun_lit (y', go body')
    | Fun_lit (y, body) -> Fun_lit (y, go body)
    | Fun_app (head, arg) -> Fun_app (go head, go arg)
  in
  go e


(** {1 Semantics} *)

let is_val (e : expr) : bool =
  match e with
  | Fun_lit _ -> true
  | _ -> false

(** {2 Evaluation} *)

let rec eval (e : expr) : expr =
  match e with
  | Var _ -> e
  | Let (x, def, body) ->
      let def = if is_val def then def else eval def in
      eval (subst (x, def) body)
  | Fun_lit (_, _) -> e
  | Fun_app (head, arg) -> begin
      let head = if is_val head then head else eval head in
      let arg = if is_val arg then arg else eval arg in
      match head with
      | Fun_lit (x, body) -> eval (subst (x, arg) body)
      | head -> Fun_app (head, arg)
  end

(** {2 Normalisation} *)

(** Fully normalise an expression, including under binders. *)
let rec normalise (e : expr) : expr =
  match e with
  | Var x -> Var x
  | Let (x, def, body) -> normalise (subst (x, normalise def) body)
  | Fun_lit (x, body) -> Fun_lit (x, normalise body)
  | Fun_app (head, arg) -> begin
      match eval head with
      | Fun_lit (x, body) -> normalise (subst (x, normalise arg) body)
      | head -> Fun_app (head, normalise arg)
  end
