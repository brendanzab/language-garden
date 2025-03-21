(** The core language. *)

type name = string
(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)


(** {1 Syntax} *)

type ty =
  | A
  | B
  | C
  | Fun_ty of ty * ty

type tm

(** {2 Pretty printing} *)

val pp_ty : Format.formatter -> ty -> unit
val pp_tm : Format.formatter -> tm -> unit


(** {1 Elaboration effect} *)

(** Total and partial elaboration effects. *)

type 'a elab
type ('a, 'e) elab_err = ('a, 'e) result elab

val run : 'a. 'a elab -> 'a


(** {1 Forms of judgement} *)

type var

type check = ty -> tm elab
type synth = (tm * ty) elab

type 'e check_err = ty -> (tm, 'e) elab_err
type 'e synth_err = (tm * ty, 'e) elab_err

(** {2 Error handling} *)

type ty_mismatch = {
  found_ty : ty;
  expected_ty : ty;
}

val fail : 'e. 'e -> 'e synth_err
val catch_check : 'e. ('e -> check) -> 'e check_err -> check
val catch_synth : 'e. ('e -> synth) -> 'e synth_err -> synth


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

val conv : synth -> [> `Type_mismatch of ty_mismatch] check_err
val ann : check -> ty -> synth

(** {2 Structural rules} *)

val var : var -> [> `Unbound_var] synth_err
val let_synth : name * ty * check -> (var -> synth) -> synth
val let_check : name * ty * check -> (var -> check) -> check

(** {2 Function rules} *)

val fun_intro_check : name * ty option -> (var -> check) -> [> `Mismatched_param_ty of ty_mismatch | `Unexpected_fun_lit of ty] check_err
val fun_intro_synth : name * ty -> (var -> synth) -> synth
val fun_elim : synth -> synth -> [> `Unexpected_arg of ty  | `Type_mismatch of ty_mismatch] synth_err
