(** The core language. *)

type name = string
(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)


(** {1 Syntax} *)

type ty
type tm

(** {2 Pretty printing} *)

val pp_ty : ty -> Format.formatter -> unit
val pp_tm : tm -> Format.formatter -> unit


(** {1 Elaboration effect} *)

(** Total and partial elaboration effects. *)

type 'a elab

val run : 'a. 'a elab -> 'a


(** {1 Forms of judgement} *)

type var

type check_tm = ty -> tm elab
type infer_tm = (tm * ty) elab


(** {2 Error handling} *)

type ty_mismatch = {
  found_ty : ty;
  expected_ty : ty;
}

type 'a error_handler = {
  run : 'b. 'a -> 'b;
}

type 'a mismatch = mismatch:ty_mismatch error_handler -> 'a
type 'a mismatched_param_ty = mismatched_param_ty:ty_mismatch error_handler -> 'a
type 'a unexpected_fun_lit = unexpected_fun_lit:ty error_handler -> 'a
type 'a unexpected_arg = unexpected_arg:ty error_handler -> 'a
type 'a mismatched_arg = mismatched_arg:ty_mismatch error_handler -> 'a


(** {1 Inference rules} *)

(** Here we encode inference rules from natural deduction:

    {v
     premise_1 ... premise_n
    ─────────────────────────
           conclusion
    v}

    As functions in the form:

    {v
    premise_1 -> ... -> premise_n -> conclusion
    v}

    Higher-order abstract syntax (HOAS) is used to represent places where new
    bindings are added to the typing context.
*)

(** {2 Directional rules} *)

val conv : infer_tm -> check_tm mismatch
val ann : check_tm -> ty -> infer_tm

(** {2 Structural rules} *)

val lookup : var -> infer_tm
val let_synth : name * ty * check_tm -> (var -> infer_tm) -> infer_tm
val let_check : name * ty * check_tm -> (var -> check_tm) -> check_tm

(** {2 Type connectives} *)

module Fun : sig

  val form : ty -> ty -> ty
  val intro_check : name * ty option -> (var -> check_tm) -> check_tm unexpected_fun_lit mismatched_param_ty
  val intro_synth : name * ty -> (var -> infer_tm) -> infer_tm
  val elim : infer_tm -> infer_tm -> infer_tm mismatched_arg unexpected_arg

end

module Int : sig

  val form : ty
  val intro : int -> infer_tm

end

module Bool : sig

  val form : ty
  val intro_true : infer_tm
  val intro_false : infer_tm
  val elim_check : check_tm -> check_tm -> check_tm -> check_tm
  val elim_synth : check_tm -> infer_tm -> check_tm -> infer_tm

end
