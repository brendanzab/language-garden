module Syntax = SurfaceSyntax

(** Elaboration context *)
type context

(** The empty {!context} *)
val initial_context : context

(** Bind a new definition in the {!context} *)
val bind_def : context -> string option -> Core.Semantics.value -> Core.Semantics.value -> context

(** Bind a new parameter in the {!context} *)
val bind_param : context -> string option -> Core.Semantics.value -> context

(** Normalise a {!Core.Syntax.term} in the given {!context} *)
val norm : context -> Core.Syntax.term -> Core.Syntax.term

(** Evaluate a {!Core.Syntax.term} in the given {!context} *)
val eval : context -> Core.Syntax.term -> Core.Semantics.value

(** Quote a {!Core.Semantics.value} back into a {!Core.Syntax.term} in the given {!context} *)
val quote : context -> Core.Semantics.value -> Core.Syntax.term


(** Check that a {!term} is a type *)
val check_ty : context -> Syntax.term -> (Core.Syntax.term, string) result

(** Check that a {!term} is an expression of a given type *)
val check_term : context -> Syntax.term -> Core.Semantics.value -> (Core.Syntax.term, string) result

(** Synthesize the type of a {!term} *)
val synth_term : context -> Syntax.term -> (Core.Syntax.term * Core.Semantics.value, string) result
