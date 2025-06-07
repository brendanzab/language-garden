(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

type ty = Core.ty =
  | A
  | B
  | C
  | Fun_ty of ty * ty

type tm =
  tm_data located
and tm_data =
  | Var of string
  | Ann of tm * ty
  | Let of string located * ty * tm * tm
  | Fun_lit of string located * ty option * tm
  | Fun_app of tm * tm


(** {1 Elaboration} *)

(* TODO: collect errors instead of failing at the first error *)

exception Error of loc * string
exception Bug of loc * string

let error loc msg = raise (Error (loc, msg))
let bug loc msg = raise (Bug (loc, msg))

type context = (string * Core.var) list

let rec check (ctx : context) (tm : tm) : Core.check =
  match tm.data with
  | Let (n, def_ty, def_tm, body_tm) ->
      Core.let_check
        (n.data, def_ty, check ctx def_tm)
        (fun v -> check ((n.data, v) :: ctx) body_tm)
  | Fun_lit (n, param_ty, body_tm) ->
      Core.fun_intro_check (n.data, param_ty) (fun v -> check ((n.data, v) :: ctx) body_tm)
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
  | Var n -> begin
      match List.assoc_opt n ctx with
      | Some i ->
          Core.var i
          |> Core.catch_synth (function
            | `Unbound_var -> bug tm.loc "unbound core variable")
      | None ->
          error tm.loc (Format.asprintf "unbound variable `%s`" n)
  end
  | Ann (tm, ty) ->
      Core.ann (check ctx tm) ty
  | Let (n, def_ty, def_tm, body_tm) ->
      Core.let_synth
        (n.data, def_ty, check ctx def_tm)
        (fun v -> synth ((n.data, v) :: ctx) body_tm)
  | Fun_lit (n, None, _) ->
      error n.loc "annotation required"
  | Fun_lit (n, Some param_ty, body_tm) ->
      Core.fun_intro_synth
        (n.data, param_ty)
        (fun v -> synth ((n.data, v) :: ctx) body_tm)
  | Fun_app (head_tm, arg_tm) ->
      Core.fun_elim (synth ctx head_tm) (synth ctx arg_tm)
      |> Core.catch_synth (function
        | `Unexpected_arg head_ty ->
            error head_tm.loc
              (Format.asprintf "unexpected argument applied to `%t`"
                (Core.pp_ty head_ty))
        | `Type_mismatch Core.{ found_ty; expected_ty } ->
            error arg_tm.loc
              (Format.asprintf "mismatched argument type, found `%t` expected `%t`"
                (Core.pp_ty found_ty)
                (Core.pp_ty expected_ty)))

let elab_check (tm : tm) (ty : ty) : Core.tm =
  Core.run (check [] tm ty)

let elab_synth (tm : tm) : Core.tm * Core.ty =
  Core.run (synth [] tm)
