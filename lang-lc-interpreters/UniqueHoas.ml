(** The lambda calculus, implemented using normalisation-by-evaluation with
    unique identifiers and higher-order abstract syntax (HOAS) in the semantic
    domain.
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
