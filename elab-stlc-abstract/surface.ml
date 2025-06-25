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

  val check_tm : tm -> Core.ty -> Core.tm
  val infer_tm : tm -> Core.tm * Core.ty

end = struct

  exception Error of loc * string

  let error loc msg = raise (Error (loc, msg))

  type context = (string * Core.var) list

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Core.Bool.form
    | Name "Int" -> Core.Int.form
    | Name name ->
        error ty.loc (Format.asprintf "unbound type `%s`" name)
    | Fun_ty (ty1, ty2) ->
        Core.Fun.form (check_ty ty1) (check_ty ty2)

  let rec check_tm (ctx : context) (tm : tm) : Core.check_tm =
    match tm.data with
    | Let (name, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_check
          (name.data, def_ty, check_tm ctx def)
          (fun v -> check_tm ((name.data, v) :: ctx) body)

    | Fun_lit (n, param_ty, body) ->
        let param_ty = Option.map check_ty param_ty in
        Core.Fun.intro_check (n.data, param_ty) (fun v -> check_tm ((n.data, v) :: ctx) body)
        (*                            ^^^^^^^^ TODO: insert a metavariable instead? *)
        |> Core.catch_check_tm begin function
          | `Unexpected_fun_lit expected_ty ->
              error tm.loc
                (Format.asprintf "found function, expected `%t`"
                  (Core.pp_ty expected_ty))
          | `Mismatched_param_ty Core.{ found_ty; expected_ty } ->
              error tm.loc
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
              error tm.loc
                (Format.asprintf "type mismatch, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end

  and infer_tm (ctx : context) (tm : tm) : Core.infer_tm =
    match tm.data with
    | Name name ->
        begin match List.assoc_opt name ctx with
        | Some var -> Core.lookup var
        | None when name = "true" -> Core.Bool.intro_true
        | None when name = "false" -> Core.Bool.intro_false
        | None -> error tm.loc (Format.asprintf "unbound variable `%s`" name)
        end

    | Let (name, def_ty, def, body) ->
        let def_ty = check_ty def_ty in
        Core.let_synth
          (name.data, def_ty, check_tm ctx def)
          (fun v -> infer_tm ((name.data, v) :: ctx) body)

    | Ann (tm, ty) ->
        let ty = check_ty ty in
        Core.ann (check_tm ctx tm) ty

    | Int_lit i ->
        Core.Int.intro i

    | Fun_lit (n, None, _) ->
        error n.loc "annotation required"

    | Fun_lit (name, Some param_ty, body) ->
        let param_ty = check_ty param_ty in
        Core.Fun.intro_synth
          (name.data, param_ty)
          (fun v -> infer_tm ((name.data, v) :: ctx) body)

    | Fun_app (head, arg) ->
        Core.Fun.elim (infer_tm ctx head) (infer_tm ctx arg)
        |> Core.catch_infer_tm begin function
          | `Unexpected_arg head_ty ->
              error head.loc
                (Format.asprintf "unexpected argument applied to `%t`"
                  (Core.pp_ty head_ty))
          | `Type_mismatch Core.{ found_ty; expected_ty } ->
              error arg.loc
                (Format.asprintf "mismatched argument type, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end

    | If_then_else (head, tm1, tm2) ->
        Core.Bool.elim_synth
          (check_tm ctx head)
          (infer_tm ctx tm1)
          (infer_tm ctx tm2)
        |> Core.catch_infer_tm begin function
          | `Mismatched_false_branch Core.{ found_ty; expected_ty } ->
              error tm2.loc
                (Format.asprintf "type mismatch, found `%t` expected `%t`"
                  (Core.pp_ty found_ty)
                  (Core.pp_ty expected_ty))
          end


  (** Public API *)

  let check_tm (tm : tm) (ty : Core.ty) : Core.tm =
    Core.run (check_tm [] tm ty)

  let infer_tm (tm : tm) : Core.tm * Core.ty =
    Core.run (infer_tm [] tm)

end
