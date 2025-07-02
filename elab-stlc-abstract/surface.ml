(** {1 Syntax} *)

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string
  | Fun_ty of ty * ty

(** Names that bind definitions or parameters *)
type binder = string spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Ann of tm * ty
  | Let of binder * ty * tm * tm
  | Fun_lit of binder * ty option * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | If_then_else of tm * tm * tm


(** Elaboration from the surface language into the core language *)
module Elab : sig

  val check_ty : ty -> (Core.ty, span * string) result
  val check_tm : tm -> Core.ty -> (Core.tm, span * string) result
  val infer_tm : tm -> (Core.tm * Core.ty, span * string) result

end = struct

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of span * string

  (** Raises an {!Error} exception *)
  let error span msg = raise (Error (span, msg))

  (** The elaboration context only needs to map names to variables. The types of
      those variables are handled internally in the {!Core} module. *)
  type context = (string * Core.var) list


  (** {2 Bidirectional type checking} *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Core.Bool.form
    | Name "Int" -> Core.Int.form
    | Name name ->
        error ty.span (Format.asprintf "unbound type `%s`" name)
    | Fun_ty (ty1, ty2) ->
        Core.Fun.form (check_ty ty1) (check_ty ty2)

  (** Elaborate a surface term into a checkable term in the core language. *)
  let rec check_tm (ctx : context) (tm : tm) : Core.check_tm =
    match tm.data with
    | Let (name, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_check
          (name.data, def_ty, check_tm ctx def)
          (fun var -> check_tm ((name.data, var) :: ctx) body)

    | Fun_lit (name, param_ty, body) ->
        Core.Fun.intro_check
          (name.data, Option.map check_ty param_ty)
          (fun var -> check_tm ((name.data, var) :: ctx) body)
        |> Core.catch_check_tm begin function
          | `Unexpected_fun_lit expected_ty ->
              error tm.span
                (Format.asprintf "found function, expected `%t`"
                  (Core.pp_ty expected_ty))
          | `Mismatched_param_ty Core.{ found_ty; expected_ty } ->
              error tm.span
                (Format.asprintf "mismatched parameter type, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end

    | If_then_else (head, tm1, tm2) ->
        Core.Bool.elim_check
          (check_tm ctx head)
          (check_tm ctx tm1)
          (check_tm ctx tm2)

    | _ ->
        Core.conv (infer_tm ctx tm)
        |> Core.catch_check_tm begin function
          | `Type_mismatch Core.{ found_ty; expected_ty } ->
              error tm.span
                (Format.asprintf "type mismatch, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end

  (** Elaborate a surface term into an inferrable term in the core language. *)
  and infer_tm (ctx : context) (tm : tm) : Core.infer_tm =
    match tm.data with
    | Name name ->
        begin match List.assoc_opt name ctx with
        | Some var -> Core.lookup var
        | None when name = "true" -> Core.Bool.intro_true
        | None when name = "false" -> Core.Bool.intro_false
        | None -> error tm.span (Format.asprintf "unbound variable `%s`" name)
        end

    | Let (name, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_synth
          (name.data, def_ty, check_tm ctx def)
          (fun var -> infer_tm ((name.data, var) :: ctx) body)

    | Ann (tm, ty) ->
        let ty = check_ty ty in
        Core.ann (check_tm ctx tm) ty

    | Int_lit i ->
        Core.Int.intro i

    | Fun_lit (n, None, _) ->
        error n.span "annotation required"

    | Fun_lit (name, Some param_ty, body) ->
        let param_ty = check_ty param_ty in
        Core.Fun.intro_synth
          (name.data, param_ty)
          (fun var -> infer_tm ((name.data, var) :: ctx) body)

    | Fun_app (head, arg) ->
        Core.Fun.elim (infer_tm ctx head) (infer_tm ctx arg)
        |> Core.catch_infer_tm begin function
          | `Unexpected_arg head_ty ->
              error head.span
                (Format.asprintf "unexpected argument applied to `%t`"
                  (Core.pp_ty head_ty))
          | `Type_mismatch Core.{ found_ty; expected_ty } ->
              error arg.span
                (Format.asprintf "mismatched argument type, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end

    | If_then_else (head, tm1, tm2) ->
        Core.Bool.elim_synth
          (check_tm ctx head)
          (infer_tm ctx tm1)
          (check_tm ctx tm2)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : unit -> a) : (a, span * string) result =
    match prog () with
    | result -> Ok result
    | exception Error (span, message) -> Error (span, message)


  (** {2 Public API} *)

  let check_ty (ty : ty) : (Core.ty, span * string) result =
    run_elab (fun () -> check_ty ty)

  let check_tm (tm : tm) (ty : Core.ty) : (Core.tm, span * string) result =
    run_elab (fun () -> Core.run (check_tm [] tm ty))

  let infer_tm (tm : tm) : (Core.tm * Core.ty, span * string) result =
    run_elab (fun () -> Core.run (infer_tm [] tm))

end
