(** {0 Logical framework} *)

(** Trusted typing rules of the core language.

   {1 References}

    - {{:https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html}
      An Algebraic Approach to Typechecking and Elaboration} by Robert Atkey
    - {{:https://gist.github.com/TOTBWF/9b2c071d2edb1c6596b785656c866fd6}
       MicroTT.ml:  A simple single-file elaborator for MLTT} by Reed Mullanix
*)

(** {1 Exceptions} *)

(** An error raised if a rule was used incorrectly, for example if a type error
    was encountered *)
exception Error of string


(** {1 Rule types} *)

type is_ty
type synth
type check

(** {2 Running rules} *)

val run_is_ty : is_ty -> Core.Level.t * Core.Syntax.tm
val run_check : Core.Semantics.vtm -> check -> Core.Syntax.tm
val run_synth : synth -> Core.Semantics.vtm * Core.Syntax.tm

(** {2 Semantics of closed terms} *)

val eval : Core.Syntax.tm -> Core.Semantics.vtm
val quote : Core.Semantics.vtm -> Core.Syntax.tm
val is_convertible : Core.Semantics.vtm -> Core.Semantics.vtm -> bool

(** {2 Converting between types of rule} *)

(** Turn a check rule into a synth rule using a type annotation. *)
val ann : ty:is_ty -> check -> synth

(** Turn a synthesis rule into a type rule. *)
val is_ty : synth -> is_ty

(** Turn a synth rule into a check rule by checking that the synthesised type
    matches the expected type. *)
val check : synth -> check


(** {1 Inference rules for the core language} *)

(** Structural rules *)
module Structure : sig

  (** Lookup a name in the context *)
  val name : string -> synth

  (** Let expressions, where the body can be synthesised *)
  val let_synth : ?name:string -> synth -> (synth -> synth) -> synth

  (** Let expressions, where the body must be checked *)
  val let_check : ?name:string -> synth -> (synth -> check) -> check

end

(** Rules related to functions *)
module Fun : sig

  (** Function introduction *)
  val intro_synth : ?name:string -> ty:is_ty -> (synth -> synth) -> synth

  (** Function introduction *)
  val intro_check : ?name:string -> ?ty:is_ty -> (synth -> check) -> check

  (** Function elimination *)
  val app : synth -> check -> synth

end

(** Rules related to universes *)
module Univ : sig

  (** Universe introduction *)
  val univ : Core.Level.t -> synth

  (** Function type introduction *)
  val fun_ : ?name:string -> synth -> (synth -> synth) -> synth

end
