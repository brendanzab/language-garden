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
  | FunTy of ty * ty

type tm =
  tm_data located
and tm_data =
  | Var of string
  | Ann of tm * ty
  | Let of string located * ty * tm * tm
  | FunLit of string located * ty option * tm
  | FunApp of tm * tm


(** {1 Elaboration} *)

(* TODO: collect errors instead of failing at the first error *)

exception Error of loc * string
exception Bug of loc * string

let error loc msg = raise (Error (loc, msg))
let bug loc msg = raise (Bug (loc, msg))

type context = (string * Core.var) list

let rec check (ctx : context) (tm : tm) : Core.check =
  fun ty ->
    match tm.data with
    | Let (n, def_ty, def_tm, body_tm) ->
        Core.let_check
          (n.data, def_ty, check ctx def_tm)
          (fun v -> check ((n.data, v) :: ctx) body_tm)
          ty
    | FunLit (n, None, body_tm) ->
        Core.fun_intro_check n.data (fun v -> check ((n.data, v) :: ctx) body_tm) ty
        |> Core.handle (function
          | Core.UnexpectedFunLit -> error tm.loc "unexpected function literal"
          | _ -> None)
    | FunLit (n, Some param_ty, body_tm) -> begin
        (* TODO: this feels messy :[ *)
        match ty with
        | FunTy (param_ty', _) when param_ty' = param_ty ->
            Core.fun_intro_check n.data (fun v -> check ((n.data, v) :: ctx) body_tm) ty
            |> Core.handle (function
              | Core.UnexpectedFunLit -> bug tm.loc "unexpected function literal"
              | _ -> None)
        | FunTy (_, _) -> error n.loc "unexpected parameter type"
        | _ -> error tm.loc "unexpected function literal"
    end
    | _ ->
        Core.conv (synth ctx tm) ty
        |> Core.handle (function
          | Core.TypeMismatch -> error tm.loc "type mismatch"
          | _ -> None)

and synth (ctx : context) (tm : tm) : Core.synth =
  match tm.data with
  | Var n -> begin
      match List.assoc_opt n ctx with
      | Some i ->
          Core.var i
          |> Core.handle (function
            | Core.UnboundVar -> bug tm.loc "unbound index"
            | _ -> None)
      | None -> error tm.loc "unbound variable"
  end
  | Ann (tm, ty) ->
      Core.ann (check ctx tm) ty
  | Let (n, def_ty, def_tm, body_tm) ->
      Core.let_synth
        (n.data, def_ty, check ctx def_tm)
        (fun v -> synth ((n.data, v) :: ctx) body_tm)
  | FunLit (_, None, _) ->
      error tm.loc "ambiguous without parameter annotation"
  | FunLit (n, Some param_ty, body_tm) ->
      Core.fun_intro_synth
        (n.data, param_ty)
        (fun v -> synth ((n.data, v) :: ctx) body_tm)
  | FunApp (head_tm, arg_tm) ->
      Core.fun_elim (synth ctx head_tm) (synth ctx arg_tm)
      |> Core.handle (function
        | Core.ExpectedFunTy -> error head_tm.loc "expected function type"
        | Core.TypeMismatch -> error arg_tm.loc "mismatch argument type"
        | _ -> None)

let elab_check (tm : tm) (ty : ty) : Core.tm =
  Core.run (check [] tm ty)

let elab_synth (tm : tm) : Core.tm * Core.ty =
  Core.run (synth [] tm)
