(** The lambda calculus, implemented using unique identifiers.

    Based on Section 4 of Lennart Augustsson’s
    {{:https://github.com/mietek/cook/blob/master/doc/pdf/augustsson-2006.pdf}
    “λ-calculus cooked four ways”}.
*)

module Id : sig
  (** Unique identifiers *)

  type t

  val fresh : unit -> t
  (** Generate a new, unique id *)

  module Map : Map.S
    with type key = t

end = struct

  type t = int

  let next_id = ref 0

  let fresh () =
    let i = !next_id in
    incr next_id;
    i

  module Map = Map.Make (Int)

end


(** {1 Syntax} *)

type expr =
  | Var of Id.t
  | Let of string * Id.t * expr * expr
  | FunLit of string * Id.t * expr
  | FunApp of expr * expr

(** {2 Conversions} *)

let of_named (e : Named.expr) : expr =
  let rec go (is : (string * Id.t) list) (e : Named.expr) : expr =
    match e with
    | Var x -> Var (List.assoc x is)
    | Let (x, def, body) -> let i = Id.fresh () in Let (x, i, go is def, go ((x, i) :: is) body)
    | FunLit (x, body) -> let i = Id.fresh () in FunLit (x, i, go ((x, i) :: is) body)
    | FunApp (head, arg) -> FunApp (go is head, go is arg)
  in
  go [] e

let to_named (e : expr) : Named.expr =
  let rec go (ns : Named.StringSet.t) (m : string Id.Map.t) (e : expr) : Named.expr =
    match e with
    | Var i -> Var (Id.Map.find i m)
    | Let (x, i, def, body) ->
        let x = Named.fresh ns x in
        Let (x, go ns m def, go (Named.StringSet.add x ns) (Id.Map.add i x m) body)
    | FunLit (x, i, body) ->
        let x = Named.fresh ns x in
        FunLit (x, go (Named.StringSet.add x ns) (Id.Map.add i x m) body)
    | FunApp (head, arg) -> FunApp (go ns m head, go ns m arg)
  in
  go Named.StringSet.empty Id.Map.empty e

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  (* Compare for alpha equivalence by comparing the binding depth of variable
    names in each expression. This approach is described in section 6.1 of
    {{:https://davidchristiansen.dk/tutorials/implementing-types-hs.pdf}
    “Checking Dependent Types with Normalization by Evaluation: A Tutorial
    (Haskell Version)”} by David Christiansen. *)
  let rec go (size : int) (ns1, e1 : int Id.Map.t * expr) (ns2, e2 : int Id.Map.t * expr) : bool =
    match e1, e2 with
    | Var i1, Var i2 -> begin
        match Id.Map.find_opt i1 ns1, Id.Map.find_opt i2 ns2 with
        | None, None -> i1 = i2
        | Some l1, Some l2  -> l1 = l2
        | _, _ -> false
    end
    | Let (_, i1, def1, body1), Let (_, i2, def2, body2) ->
        go size (ns1, def1) (ns2, def2)
          && go (size + 1) (Id.Map.add i1 size ns1, body1) (Id.Map.add i2 size ns2, body2)
    | FunLit (_, i1, body1), FunLit (_, i2, body2) ->
        go (size + 1) (Id.Map.add i1 size ns1, body1) (Id.Map.add i2 size ns2, body2)
    | FunApp (head1, arg1), FunApp (head2, arg2) ->
        go size (ns1, head1) (ns2, head2) && go size (ns1, arg1) (ns2, arg2)
    | _, _ -> false
  in
  go 0 (Id.Map.empty, e1) (Id.Map.empty, e2)

(** {2 Substitution} *)

let subst (i, s : Id.t * expr) (e : expr) : expr =
  let clone (e : expr) : expr =
    (* Rename all binders with fresh ids *)
    let rec rename (m : Id.t Id.Map.t) (e : expr) : expr =
      match e with
      | Var i -> begin
          match Id.Map.find_opt i m with
          | None -> e
          | Some i -> Var i
      end
      | Let (x, i, def, body) ->
          let i' = Id.fresh () in
          Let (x, i', rename m def, rename (Id.Map.add i i' m) body)
      | FunLit (x, i, body) ->
          let i' = Id.fresh () in
          FunLit (x, i', rename (Id.Map.add i i' m) body)
      | FunApp (head, arg) ->
          FunApp (rename m head, rename m arg)
    in
    (* Rename only terms that include binders *)
    let rec rename_binders (e : expr) : expr option =
      match e with
      | Var _ -> None
      | Let _ | FunLit _ ->
          Some (rename Id.Map.empty e)
      | FunApp (head, arg) -> begin
          match rename_binders head, rename_binders arg with
          | Some head, None -> Some (FunApp (head, arg))
          | None, Some arg -> Some (FunApp (head, arg))
          | Some head, Some arg -> Some (FunApp (head, arg))
          | None, None -> None
      end
    in
    rename_binders e
    |> Option.value ~default:e
  in
  let rec go (e : expr) : expr =
    match e with
    | Var j -> if i = j then clone s else e
    | Let (x, j, def, body) -> Let (x, j, go def, go body)
    | FunLit (x, j, body) -> FunLit (x, j, go body)
    | FunApp (head, arg) -> FunApp (go head, go arg)
  in
  go e


(** {1 Semantics} *)

let is_val (e : expr) : bool =
  match e with
  | FunLit _ -> true
  | _ -> false

(** {2 Evaluation} *)

let rec eval (e : expr) : expr =
  match e with
  | Var _ -> e
  | Let (_, i, def, body) ->
      let def = if is_val def then def else eval def in
      eval (subst (i, def) body)
  | FunLit (_, _, _) -> e
  | FunApp (head, arg) -> begin
      let head = if is_val head then head else eval head in
      let arg = if is_val arg then arg else eval arg in
      match head with
      | FunLit (_, i, body) -> eval (subst (i, arg) body)
      | head -> FunApp (head, arg)
  end

(** {2 Normalisation} *)

(** Fully normalise an expression, including under binders. *)
let rec normalise (e : expr) : expr =
  match e with
  | Var x -> Var x
  | Let (_, i, def, body) -> normalise (subst (i, normalise def) body)
  | FunLit (x, i, body) -> FunLit (x, i, normalise body)
  | FunApp (head, arg) -> begin
      match eval head with
      | FunLit (_, i, body) -> normalise (subst (i, normalise arg) body)
      | head -> FunApp (head, normalise arg)
  end
