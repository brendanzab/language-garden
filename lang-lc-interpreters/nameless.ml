(** The lambda calculus, implemented using de Bruijn indices, as demonstrated in
    “Types and Programming Languages”.

    This approach makes alpha-equivalence trivial at the expense of literally
    everything else. It’s expensive, requiring lots of tree-traversals and
    transient memory allocations, and error prone (it’s very easy to forget to
    shift a de Bruijn index). I don’t recommend it for most implementations.
    See {!Nameless_closures} and {!Nameless_hoas} for far better approaches that
    use de Bruijn indices.
*)

(** [elem_index x xs] returns the index of the first occurance of [x] in [xs]. *)
let elem_index (type a) (a : a) (xs : a list) =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0 xs


(** {1 Syntax} *)

(**  De Bruijn index *)
type index = int

type expr =
  | Var of index
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr

(** {2 Conversions} *)

let of_named (e : Named.expr) : expr =
  let rec go (ns : string list) (e : Named.expr) : expr =
    match e with
    | Var x -> Var (elem_index x ns |> Option.get)
    | Let (x, def, body) -> Let (x, go ns def, go (x :: ns) body)
    | FunLit (x, body) -> FunLit (x, go (x :: ns) body)
    | FunApp (head, arg) -> FunApp (go ns head, go ns arg)
  in
  go [] e

let to_named (e : expr) : Named.expr =
  let rec fresh (ns : string list) (x : string) : string =
    match List.mem x ns with
    | true -> fresh ns (x ^ "'")
    | false -> x
  in
  let rec go (ns : string list) (e : expr) : Named.expr =
    match e with
    | Var i -> Var (List.nth ns i)
    | Let (x, def, body) -> let x = fresh ns x in Let (x, go ns def, go (x :: ns) body)
    | FunLit (x, body) -> let x = fresh ns x in FunLit (x, go (x :: ns) body)
    | FunApp (head, arg) -> FunApp (go ns head, go ns arg)
  in
  go [] e

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

(** {2 Updating variables} *)

let map_vars (on_var : int -> int -> expr) (size : int) (e : expr) : expr =
  let rec go (size : int) (e : expr) : expr =
    match e with
    | Var i -> on_var size i
    | Let (x, def, body) -> Let (x, go size def, go (size + 1) body)
    | FunLit (x, body) -> FunLit (x, go (size + 1) body)
    | FunApp (e1, e2) -> FunApp (go size e1, go size e2)
  in
  go size e

let shift (diff : int) (e : expr) : expr =
  map_vars (fun size i -> if i >= size then Var (i + diff) else Var i) 0 e

let subst (i, s : int * expr) (e : expr) : expr =
  map_vars (fun size j -> if i = j + size then shift size s else e) 0 e

let subst_top (s : expr) (e : expr) : expr =
  shift (-1) (subst (0, shift 1 s) e)


(** {1 Semantics} *)

let is_val (e : expr) : bool =
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
