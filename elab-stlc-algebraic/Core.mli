(** The core language. *)

type name = string
(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)


(** {1 Syntax} *)

type ty =
  | A
  | B
  | C
  | FunTy of ty * ty

type tm

(** {2 Pretty printing} *)

val pp_ty : Format.formatter -> ty -> unit
val pp_tm : Format.formatter -> tm -> unit


(** {1 Elaboration effect} *)

type 'a elab
(** Elaboration effect *)

val run : 'a elab -> 'a
(** Run an elaboration computation assuming an empty context *)

(** {2 Error handling} *)

exception UnboundVar
exception UnexpectedFunLit
exception UnexpectedArg of { head_ty : ty }
exception TypeMismatch of { found_ty : ty; expected_ty : ty }

val fail : exn -> 'a elab

val handle : (exn -> 'a elab option) -> 'a elab -> 'a elab


(** {1 Forms of judgement} *)

type var

type check = ty -> tm elab
(** Produce a term given a type annotation *)

type synth = (tm * ty) elab
(** Produce a term and a type annotation *)


(** {1 Inference rules} *)

(** Trusted interface for constructing terms in the core language. *)


(** {2 Directional rules} *)

val conv : synth -> check
(** Turn a synthesisable term into a checkable term

    @raises TypeMismatch if the synthesised type did not match the type being
            checked against
*)

val ann : check -> ty -> synth
(** Annotate a checkable term with a type *)

(** {2 Structural rules} *)

val var : var -> synth
(** Variable rule

    @raises UnboundVar if the variable was not bound
*)

val let_synth : name * ty * check -> (var -> synth) -> synth
(** Synthesisable let bindings *)

val let_check : name * ty * check -> (var -> check) -> check
(** Checkable let bindings *)

(** {2 Function rules} *)

val fun_intro_check : name -> (var -> check) -> check
(** Function introduction

    @raises UnexpectedFunLit if a function was not expected
*)

val fun_intro_synth : name * ty -> (var -> synth) -> synth
(** Function introduction with an explicit parameter type *)

val fun_elim : synth -> synth -> synth
(** Function elimination

    @raises UnexpectedArg if the head is not a function type
    @raises TypeMismatch if the type of the argument does not match the type of
            the parameter
*)
