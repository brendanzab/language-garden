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


  (** The type of the continuation *)
  type 'a cont = 'a -> Anf_lang.expr

  (** Construct a continuation-passing computation from a function *)
  val embed : 'a. 'a cont cont -> 'a t

  (** Run a continuation-passing computation with a final continuation,
      returning the result. This is the inverse of {!embed}. *)
  val run : 'a. 'a t -> 'a cont cont


  (** Translate from an arithmetic expression to a computation that constructs
      an ANF expression with a continuation.  *)
  include Translation.S
    with type source = Tree_lang.expr
    with type target = Anf_lang.comp t

end
