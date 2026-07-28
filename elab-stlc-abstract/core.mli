(** The core language. *)

type name = string
(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)


(** {1 Syntax} *)

type ty
type tm

val normalise : tm -> tm

(** {2 Pretty printing} *)

val pp_ty : ty -> Format.formatter -> unit
val pp_tm : tm -> Format.formatter -> unit

(** {1 Forms of judgement} *)

type var

type check_tm
type infer_tm

type 'e check_tm_err
type 'e infer_tm_err

val run_check_tm : check_tm -> ty -> tm
val run_infer_tm : infer_tm -> tm * ty

(** {2 Error handling} *)

val fail : 'e -> 'e infer_tm_err
val catch_check_tm : ('e -> check_tm) -> 'e check_tm_err -> check_tm
val catch_infer_tm : ('e -> infer_tm) -> 'e infer_tm_err -> infer_tm


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

type conv_err =
  | Type_mismatch of { found_ty : ty; expected_ty : ty }

val conv : infer_tm -> conv_err check_tm_err
val ann : check_tm -> ty -> infer_tm

(** {2 Structural rules} *)

val lookup : var -> infer_tm
val let_synth : name * infer_tm -> (var -> infer_tm) -> infer_tm
val let_check : name * infer_tm -> (var -> check_tm) -> check_tm

(** {2 Type connectives} *)

module Fun : sig

  type intro_check_err =
    | Mismatched_param_ty of { found_ty : ty; expected_ty : ty }
    | Unexpected_fun_lit of { expected_ty : ty }

  type elim_err =
    | Unexpected_arg of { fun_ty : ty }

  val form : ty -> ty -> ty
  val intro_check : name * ty option -> (var -> check_tm) -> intro_check_err check_tm_err
  val intro_synth : name * ty -> (var -> infer_tm) -> infer_tm
  val elim : infer_tm -> check_tm -> elim_err infer_tm_err

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
