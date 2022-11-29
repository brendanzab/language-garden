(** {0 Translation from the core language to the stratified language} *)

(** Translation environments *)
type env

(** The initial, empty environment *)
val empty_env : env

(** A translated term *)
type tm = [
  | `Tm2 of Stratified.Syntax.tm2  (** Level 2 terms *)
  | `Tm1 of Stratified.Syntax.tm1  (** Level 1 terms *)
  | `Tm0 of Stratified.Syntax.tm0  (** Level 0 terms *)
]

(** Translate a term from the core language to a term in the stratified language *)
val translate : env -> Core.Syntax.tm -> tm
