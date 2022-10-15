(** Translation pass between the {!TreeLang} and {!AnfLang} *)


(** Translate from an arithmetic expression to an ANF expression *)
include Translation.S
  with type source = TreeLang.expr
  with type target = AnfLang.expr


(** An environment for constructing ANF expressions *)
module Env : sig

  (** An effectful computation *)
  type 'a t

  (** Embed a pure value in a computation *)
  val pure : 'a -> 'a t

  (** Apply the result of a computation to a function *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t


  (** An environment for mapping bindings in the tree language to atomic
      expressions in the ANF language. *)
  type env

  (** Any empty environment with no bindings. *)
  val empty : env

  (** The type of the continuation *)
  type 'a cont = env -> 'a -> AnfLang.expr

  (** Construct a continuation-passing computation from a function *)
  val embed : 'a cont cont -> 'a t

  (** Run a continuation-passing computation with a final continuation,
      returning the result. This is the inverse of {!embed}. *)
  val run : 'a t -> 'a cont cont

  (** Access the current environment *)
  val get_env : env t

  (** Get the atom bound for the given variable in the tree language *)
  val get_var : TreeLang.index -> AnfLang.atom t


  (** Translate from an arithmetic expression to a computation that constructs
      an ANF expression with a continuation. *)
  include Translation.S
    with type source = TreeLang.expr
    with type target = AnfLang.comp t

end
