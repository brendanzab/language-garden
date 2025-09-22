(** Genralised derivatives of data structures and tail-recursive transformations,
    based on Conor McBride’s “Clowns to the Left of me, Jokers to the Right” paper.

    - https://doi.org/10.1145/1328438.1328474
    - https://strictlypositive.org/CJ.pdf
    - https://reasonablypolymorphic.com/blog/clowns-jokers/index.html
    - https://gist.github.com/isovector/8a02f5c21bdb5ff5034b661ef9d28d10
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-type-declaration"]

(** Naive evaluator (not tail-recursive) *)
module Eval = struct

  type expr =
    | Lit of int
    | Add of expr * expr

  let rec eval : expr -> int =
    function
    | Lit i -> i
    | Add (e1, e2) -> eval e1 + eval e2

end


(** Tail recursive evaluator  *)
module Eval_machine = struct

  type expr =
    | Lit of int
    | Add of expr * expr

  (** Each layer in the stack is a “dissection” of [expr]’s recursion pattern.
      There are two ways to be stuck in the middle of evaluating an expression -
      either:

      - on the [Left], with [expr]s waiting to the right of us
      - on the [Right], with an [int] cached to the left of us

      This feels a bit like the (K)ontinuation in a CK or CEK machine?
  *)
  type stack = (expr, int) Either.t list

  (** Stuck in the middle with an expression to decompose, loading the stack by
      going left *)
  let rec load : expr -> stack -> int =
    fun e stk ->
      match e with
      | Lit i -> (unload [@tailcall]) i stk
      | Add (e1, e2) -> (load [@tailcall]) e1 (Left e2 :: stk)

  (** Stuck in the middle with a value to use, unloading the stack and moving
      right *)
  and unload : int -> stack -> int =
    fun v stk ->
      match v, stk with
      | v, [] -> v
      | v1, Left e2 :: stk -> (load [@tailcall]) e2 (Right v1 :: stk)
      | v2, Right v1 :: stk -> (unload [@tailcall]) (v1 + v2) stk

  let eval : expr -> int =
    fun e ->
      load e []

end


module Void = struct

  type t = |

  let absurd : type a. t -> a =
    function _ -> .

end


(* Type constructors *)

module type T0 = sig type t end
module type T1 = sig type 'a t end
module type T2 = sig type ('a, 'b) t end


module Functor = struct

  module type S = sig

    include T1

    val map : ('a -> 'b) -> 'a t -> 'b t

  end

  module Ops (F : S) = struct

    (** [inflate x] is equivalent to [F.map Void.magic x], but avoids traversing
        the data structure. *)
    let inflate : type a. Void.t F.t -> a F.t =
      fun x ->
        Obj.magic x

  end

end

module Fix (P : Functor.S) : sig

  type t = In of t P.t

  val fold : ('v P.t -> 'v) -> t -> 'v

end = struct

  type t = In of t P.t

  let rec fold : type v. (v P.t -> v) -> t -> v =
    fun f (In p) ->
      f (P.map (fold f) p)

end

module Bifunctor = struct

  module type S = sig

    include T2

    val bimap : ('a1 -> 'b1) -> ('a2 -> 'b2) -> ('a1, 'a2) t -> ('b1, 'b2) t

  end

end


(** Polynomial type constructors in one parameter

    Unlike Haskell, we’ll be using modules instead of type classes, so we
    don’t need to define new constructors for [Const1], [Id], etc. and instead
    just use type aliases.
*)
module Polynomial_functors = struct

  module Const1 (A : T0) : Functor.S
    with type 'x t = A.t
  = struct

    type 'x t = A.t

    let map : type a b. (a -> b) -> a t -> b t =
      fun _ a -> a

  end

  module Id : Functor.S
    with type 'x t = 'x
  = struct

    type 'x t = 'x

    let map : type a b. (a -> b) -> a t -> b t =
      fun f x -> f x

  end

  module Either1 (P : Functor.S) (Q : Functor.S) : Functor.S
    with type 'x t = ('x P.t, 'x Q.t) Either.t
  = struct

    type 'x t = ('x P.t, 'x Q.t) Either.t

    let map : type a b. (a -> b) -> a t -> b t =
      fun f pq ->
        match pq with
        | Left p -> Left (P.map f p)
        | Right q -> Right (Q.map f q)

  end

  module Pair1 (P : Functor.S) (Q : Functor.S) : Functor.S
    with type 'x t = 'x P.t * 'x Q.t
  = struct

    type 'x t = 'x P.t * 'x Q.t

    let map : type a b. (a -> b) -> a t -> b t =
      fun f (p, q) ->
        P.map f p, Q.map f q

  end


  (** Common abbreviations *)

  module Unit1 : Functor.S with type 'x t = unit =
    Const1 (Unit)

  module Option : sig

    type 'a t

    include Functor.S
      with type 'a t := 'a t

    val none : 'a t
    val some : 'a -> 'a t
    val elim : 'a t -> [> `None | `Some of 'a ]

  end = struct

    include Either1 (Unit1) (Id)

    let none = Either.Left ()
    let some x = Either.Right x

    let elim = function
      | Either.Left () -> `None
      | Either.Right x -> `Some x

  end


  (** Evaluation of expressions *)

  module Expr : sig

    module Layer : sig

      type 'a t

      include Functor.S
        with type 'a t := 'a t

      val lit : int -> 'a t
      val add : 'a -> 'a -> 'a t
      val elim : 'a t -> [> `Lit of int | `Add of 'a * 'a ]

    end

    include module type of Fix (Layer)

    val lit : int -> t
    val add : t -> t -> t
    val eval : t -> int

  end = struct

    module Layer = struct

      include Either1 (Const1 (Int)) (Pair1 (Id) (Id))

      let lit i = Either.Left i
      let add e1 e2 = Either.Right (e1, e2)

      let elim = function
        | Either.Left i -> `Lit i
        | Either.Right (e1, e2) -> `Add (e1, e2)

    end

    include Fix (Layer)

    let lit i = In (Layer.lit i)
    let add e1 e2 = In (Layer.add e1 e2)

    let eval : t -> int =
      fold @@ fun x ->
        match Layer.elim x with
        | `Lit i -> i
        | `Add (v1, v2) -> v1 + v2

  end

end


(** Polynomial type constructors in two parameters parameter *)
module Polynomial_bifunctors = struct

  module Const2 (A : T0) : Bifunctor.S
    with type ('x, 'y) t = A.t
  = struct

    type ('x, 'y) t = A.t

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun _ _ a -> a

  end

  module Fst : Bifunctor.S
    with type ('x, 'y) t = 'x
  = struct

    type ('x, 'y) t = 'x

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun f1 _ x -> f1 x

  end

  module Snd : Bifunctor.S
    with type ('x, 'y) t = 'y
  = struct

    type ('x, 'y) t = 'y

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun _ f2 y -> f2 y

  end

  module Either2 (P : Bifunctor.S) (Q : Bifunctor.S) : Bifunctor.S
    with type ('x, 'y) t = (('x, 'y) P.t, ('x, 'y) Q.t) Either.t
  = struct

    type ('x, 'y) t = (('x, 'y) P.t, ('x, 'y) Q.t) Either.t

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun f1 f2 pq ->
        match pq with
        | Left p -> Left (P.bimap f1 f2 p)
        | Right q -> Right (Q.bimap f1 f2 q)

  end

  module Pair2 (P : Bifunctor.S) (Q : Bifunctor.S) : Bifunctor.S
    with type ('x, 'y) t = ('x, 'y) P.t * ('x, 'y) Q.t
  = struct

    type ('x, 'y) t = ('x, 'y) P.t * ('x, 'y) Q.t

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun f1 f2 (p, q) ->
        P.bimap f1 f2 p, Q.bimap f1 f2 q

  end


  (** Common abbreviations *)

  module Unit2 : Bifunctor.S with type ('x, 'y) t = unit = Const2 (Unit)
  module Void1 : Functor.S with type 'x t = Void.t = Polynomial_functors.Const1 (Void)
  module Void2 : Bifunctor.S with type ('x, 'y) t = Void.t = Const2 (Void)


  (** Clowns, Jokers and Dissection *)

  module Clowns (* to the left *) (P : Functor.S) : Bifunctor.S
    with type ('c, 'j) t = 'c P.t
  = struct

    type ('c, 'j) t = 'c P.t

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun f1 _ pc ->
        P.map f1 pc

  end

  module Jokers (* to the right *) (P : Functor.S) : Bifunctor.S
    with type ('c, 'j) t = 'j P.t
  = struct

    type ('c, 'j) t = 'j P.t

    let bimap : type a1 a2 b1 b2. (a1 -> b1) -> (a2 -> b2) -> (a1, a2) t -> (b1, b2) t =
      fun _ f2 pj ->
        P.map f2 pj

  end


  module Dissect = struct

    module type S = sig

      include Functor.S

      (** The state of a traversal over [t], with a hole in it *)
      module State : Bifunctor.S

      (** Creep gradually to the right *)
      val right : ('j t, ('c, 'j) State.t * 'c) Either.t -> (('j * ('c, 'j) State.t), 'c t) Either.t

      (** If the clowns and jokers coincide the hole can be filled directly
          without needing to traverse all the way to the end. *)
      val plug : 'x -> ('x, 'x) State.t -> 'x t

    end

    (* NOTE: Will work better with [include functor]
       https://github.com/ocaml/RFCs/pull/43 *)
    module Tailrec (P : S) = struct

      (** Tail-recursive map *)
      let map : type a b. (a -> b) -> a P.t -> b P.t =
        fun f ps ->
          let rec continue = function
            | Either.Left (s, pd) ->
                (continue [@tailcall]) (P.right (Either.Right (pd, f s)))
            | Either.Right pt -> pt
          in
          continue (P.right (Either.Left ps))

      let divide : type x. x P.t -> (x * (Void.t, x) P.State.t, Void.t P.t) Either.t =
        fun px ->
          P.right (Either.Left px)

      (* TODO: inflate_fst *)
      (* TODO: unite *)

    end

  end

  module Const1 (A : T0) : Dissect.S
    with type 'x t = A.t
    with type ('x, 'y) State.t = ('x, 'y) Void2.t
  = struct

    include Polynomial_functors.Const1 (A)

    module State = Void2

    let right : type c j. (j t, (c, j) State.t * c) Either.t -> ((j * (c, j) State.t), c t) Either.t =
      function
      | Left a -> Right a   (* jump all the way from far left to far right in one go *)
      | Right (_, _) -> .   (* cannot be in the middle, so refute that case *)

    let plug : type x. x -> (x, x) State.t -> x t =
      fun _ z -> Void.absurd z

  end

  module Id : Dissect.S
    with type 'x t = 'x
    with type ('x, 'y) State.t = ('x, 'y) Unit2.t
  = struct

    include Polynomial_functors.Id

    module State = Unit2

    let right : type c j. (j t, (c, j) State.t * c) Either.t -> ((j * (c, j) State.t), c t) Either.t =
      function
      | Left j -> Left (j, ())   (* step into a single element *)
      | Right ((), c) -> Right c (* step out *)

    let plug : type x. x -> (x, x) State.t -> x t =
      fun x () -> x

  end

  module Either1 (P : Dissect.S) (Q : Dissect.S) : Dissect.S
    with type 'x t = ('x P.t, 'x Q.t) Either.t
    with type ('x, 'y) State.t = (('x, 'y) P.State.t, ('x, 'y) Q.State.t) Either.t
  = struct

    open Either

    include Polynomial_functors.Either1 (P) (Q)

    module State = Either2 (P.State) (Q.State)

    let right : type c j. (j t, (c, j) State.t * c) Either.t -> ((j * (c, j) State.t), c t) Either.t =
      let mindp = function
        | Left (j, pd) -> Left (j, Left pd)
        | Right pc -> Right (Left pc)
      and mindq = function
        | Left (j, qd) -> Left (j, Right qd)
        | Right qc -> Right (Right qc)
      in
      function
      | Left (Left pj) -> mindp (P.right (Left pj))
      | Left (Right qj) -> mindq (Q.right (Left qj))
      | Right (Left pd, c) -> mindp (P.right (Right (pd, c)))
      | Right (Right qd, c) -> mindq (Q.right (Right (qd, c)))

    let plug : type x. x -> (x, x) State.t -> x t =
      fun x pqd ->
        match pqd with
        | Left pd -> Left (P.plug x pd)
        | Right qd -> Right (Q.plug x qd)

  end

  module Pair1 (P : Dissect.S) (Q : Dissect.S) : Dissect.S
    with type 'x t = 'x P.t * 'x Q.t
    with type ('x, 'y) State.t =
      (('x, 'y) P.State.t * 'y Q.t,
        'x P.t * ('x, 'y) Q.State.t) Either.t
  = struct

    open Either

    include Polynomial_functors.Pair1 (P) (Q)

    module State =
      Either2
        (Pair2 (P.State) (Jokers (Q)))
        (Pair2 (Clowns (P)) (Q.State))

    let right : type c j. (j t, (c, j) State.t * c) Either.t -> ((j * (c, j) State.t), c t) Either.t =
      let rec mindp = function
        | Left (j, pd), qj -> Left (j, Left (pd, qj))
        | Right pc, qj -> mindq (pc, Q.right (Left qj))
      and mindq = function
        | pc, Left (j, qd) -> Left (j, Right (pc, qd))
        | pc, Right qc -> Right (pc, qc)
      in
      function
      | Left (pj, qj) -> mindp (P.right (Left pj), qj)
      | Right (Left (pd, qj), c) -> mindp (P.right (Right (pd, c)), qj)
      | Right (Right (pc, qd), c) -> mindq (pc, Q.right (Right (qd, c)))

    let plug : type x. x -> (x, x) State.t -> x t =
      fun x pqd ->
        match pqd with
        | Left (pd, qx) -> P.plug x pd, qx
        | Right (px, qd) -> px, Q.plug x qd

  end


  module Fix (P : Dissect.S) : sig

    type t = Fix (P).t = In of t P.t

    (** Tail-recursive fold *)
    val fold : ('v P.t -> 'v) -> t -> 'v

    (** Zipper operations *)

    val up : t * (t, t) P.State.t list -> (t * (t, t) P.State.t list) option
    val down : t * (t, t) P.State.t list -> (t * (t, t) P.State.t list) option
    val left : t * (t, t) P.State.t list -> (t * (t, t) P.State.t list) option
    val right : t * (t, t) P.State.t list -> (t * (t, t) P.State.t list) option

  end = struct

    type t = Fix (P).t = In of t P.t

    type 'v stack = ('v, t) P.State.t list

    let rec load : type v. (v P.t -> v) -> t -> v stack -> v =
      fun f (In pt) stk ->
        (next [@tailcall]) f (P.right (Left pt)) stk

    and next : type v. (v P.t -> v) -> (t * (v, t) P.State.t, v P.t) Either.t -> v stack -> v =
      fun f p stk ->
        match p with
        | Left (t, pd) -> (load [@tailcall]) f t (pd :: stk)
        | Right pv -> (unload [@tailcall]) f (f pv) stk

    and unload : type v. (v P.t -> v) -> v -> v stack -> v =
      fun f v stk ->
        match stk with
        | pd :: stk -> (next [@tailcall]) f (P.right (Right (pd, v))) stk
        | [] -> v

    (** Tail-recursive fold *)
    let fold : type v. (v P.t -> v) -> t -> v =
      fun f p ->
        load f p []


    (** Zipper operations *)

    let up : (t * (t, t) P.State.t list) -> (t * (t, t) P.State.t list) option =
      function
      | _, [] -> None
      | t, pd :: pds -> Some (In (P.plug t pd), pds)

    let down : (t * (t, t) P.State.t list) -> (t * (t, t) P.State.t list) option =
      fun (In pt, pds) ->
        match P.right (Left pt) with
        | Left (t, pd) -> Some (t, pd :: pds)
        | Right _ -> None

    let left : (t * (t, t) P.State.t list) -> (t * (t, t) P.State.t list) option =
      function
      | _, _ ->
          failwith "TODO"

    let right : (t * (t, t) P.State.t list) -> (t * (t, t) P.State.t list) option =
      function
      | _, [] -> None
      | t, pd :: pds ->
          match P.right (Right (pd, t)) with
          | Left (t', pd') -> Some (t', pd' :: pds)
          | Right _ -> None

  end


  (** Evaluation of expressions. This is the same as in [Polynomial_functors],
      but this time the traversal is tail-recursive. *)

  module Expr : sig

    module Layer : sig

      type 'a t

      include Dissect.S
        with type 'a t := 'a t

      val lit : int -> 'a t
      val add : 'a -> 'a -> 'a t
      val elim : 'a t -> [> `Lit of int | `Add of 'a * 'a ]

    end

    include module type of Fix (Layer)

    val lit : int -> t
    val add : t -> t -> t
    val eval : t -> int

  end = struct

    module Layer = struct

      include Either1 (Const1 (Int)) (Pair1 (Id) (Id))

      let lit i = Either.Left i
      let add e1 e2 = Either.Right (e1, e2)

      let elim = function
        | Either.Left i -> `Lit i
        | Either.Right (e1, e2) -> `Add (e1, e2)

    end

    include Fix (Layer)

    let lit i = In (Layer.lit i)
    let add e1 e2 = In (Layer.add e1 e2)

    let eval : t -> int =
      fold @@ fun x ->
        match Layer.elim x with
        | `Lit i -> i
        | `Add (v1, v2) -> v1 + v2

  end

end
