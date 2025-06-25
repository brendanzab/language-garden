(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | Fun_ty of ty * ty

type tm =
  tm_data located
and tm_data =
  | Name of string
  | Ann of tm * ty
  | Let of string located * ty * tm * tm
  | Fun_lit of string located * ty option * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | If_then_else of tm * tm * tm


(** Elaboration from the surface language into the core language *)
module Elab : sig

  (* TODO: collect errors instead of failing at the first error *)

  exception Error of loc * string
  exception Bug of loc * string

  val check : tm -> Core.ty -> Core.tm
  val synth : tm -> Core.tm * Core.ty

end = struct

  exception Error of loc * string
  exception Bug of loc * string

  let error loc msg = raise (Error (loc, msg))
  let bug loc msg = raise (Bug (loc, msg))

  type context = (string * Core.var) list

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Core.bool_form
    | Name "Int" -> Core.int_form
    | Name name ->
        error ty.loc (Format.asprintf "unbound type `%s`" name)
    | Fun_ty (ty1, ty2) ->
        Core.fun_form (check_ty ty1) (check_ty ty2)

  let rec check (ctx : context) (tm : tm) : Core.check =
    match tm.data with
    | Let (n, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_check
          (n.data, def_ty, check ctx def)
          (fun v -> check ((n.data, v) :: ctx) body)
    | Fun_lit (n, param_ty, body) ->
        let param_ty = Option.map check_ty param_ty in
        Core.fun_intro_check (n.data, param_ty) (fun v -> check ((n.data, v) :: ctx) body)
        (*                            ^^^^^^^^ TODO: insert a metavariable instead? *)
        |> Core.catch_check (function
          | `Unexpected_fun_lit expected_ty ->
              error tm.loc
                (Format.asprintf "found function, expected `%t`"
                  (Core.pp_ty expected_ty))
          | `Mismatched_param_ty Core.{ found_ty; expected_ty } ->
              error tm.loc
                (Format.asprintf "mismatched parameter type, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty)))
    | If_then_else (head, tm1, tm2) ->
        Core.bool_elim_check
          (check ctx head)
          (check ctx tm1)
          (check ctx tm2)
    | _ ->
        Core.conv (synth ctx tm)
        |> Core.catch_check (function
          | `Type_mismatch Core.{ found_ty; expected_ty } ->
              error tm.loc
                (Format.asprintf "type mismatch, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty)))

  and synth (ctx : context) (tm : tm) : Core.synth =
    match tm.data with
    | Name name -> begin
        match List.assoc_opt name ctx with
        | Some index ->
            Core.var index
            |> Core.catch_synth (function
              | `Unbound_var -> bug tm.loc "unbound core variable")
        | None when name = "true" -> Core.bool_true
        | None when name = "false" -> Core.bool_false
        | None -> error tm.loc (Format.asprintf "unbound variable `%s`" name)
    end
    | Let (n, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_synth
          (n.data, def_ty, check ctx def)
          (fun v -> synth ((n.data, v) :: ctx) body)
    | Ann (tm, ty) ->
        let ty = check_ty ty in
        Core.ann (check ctx tm) ty
    | Int_lit i ->
        Core.int_intro i
    | Fun_lit (n, None, _) ->
        error n.loc "annotation required"
    | Fun_lit (n, Some param_ty, body) ->
        let param_ty = check_ty param_ty in
        Core.fun_intro_synth
          (n.data, param_ty)
          (fun v -> synth ((n.data, v) :: ctx) body)
    | Fun_app (head, arg) ->
        Core.fun_elim (synth ctx head) (synth ctx arg)
        |> Core.catch_synth (function
          | `Unexpected_arg head_ty ->
              error head.loc
                (Format.asprintf "unexpected argument applied to `%t`"
                  (Core.pp_ty head_ty))
          | `Type_mismatch Core.{ found_ty; expected_ty } ->
              error arg.loc
                (Format.asprintf "mismatched argument type, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty)))
    | If_then_else (head, tm1, tm2) ->
        Core.bool_elim_synth
          (check ctx head)
          (synth ctx tm1)
          (synth ctx tm2)
        |> Core.catch_synth (function
          | `Mismatched_branches Core.{ found_ty; expected_ty } ->
              error tm2.loc
                (Format.asprintf "type mismatch, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty)))


  (** Public API *)

  let check (tm : tm) (ty : Core.ty) : Core.tm =
    Core.run (check [] tm ty)

  let synth (tm : tm) : Core.tm * Core.ty =
    Core.run (synth [] tm)

end
