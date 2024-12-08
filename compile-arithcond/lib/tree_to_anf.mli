(** Translation pass between the {!Tree_lang} and {!Anf_lang} *)


(** Translate from an arithmetic expression to an ANF expression *)
include Translation.S
  with type source = Tree_lang.expr
  with type target = Anf_lang.expr


(** An environment for constructing ANF expressions *)
module Env : sig

  (** An effectful computation *)
  type 'a t

  (** Embed a pure value in a computation *)
  val pure : 'a. 'a -> 'a t

  (** Apply the result of a computation to a function *)
  val bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t


  (** An environment for mapping bindings in the tree language to atomic
      expressions in the ANF language. *)
  type env

  (** Any empty environment with no bindings. *)
  val empty : env

  (** The type of the continuation *)
  type 'a cont = env -> 'a -> Anf_lang.expr

  (** Construct a continuation-passing computation from a function *)
  val embed : 'a. 'a cont cont -> 'a t

  (** Run a continuation-passing computation with a final continuation,
      returning the result. This is the inverse of {!embed}. *)
  val run : 'a. 'a t -> 'a cont cont

  (** Access the current environment *)
  val get_env : env t

  (** Get the atom bound for the given variable in the tree language *)
  val get_var : Tree_lang.index -> Anf_lang.atom t


  (** Translate from an arithmetic expression to a computation that constructs
      an ANF expression with a continuation. *)
  include Translation.S
    with type source = Tree_lang.expr
    with type target = Anf_lang.comp t

end
