(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder = string located

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | Fun of ty * ty

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Add of tm * tm
  | Template of template
  | IfThenElse of tm * tm * tm
  | App of tm * tm
  | TextLit of string
  | IntLit of int

(** Templates *)
and template =
  fragment list

(** Template fragments *)
and fragment =
  fragment_data located

and fragment_data =
  | TextFragment of string
  | TermFragment of tm
  | LetFragment of binder * param list * ty option * tm
  | IfThenElseFragment of tm * template * template

and param =
  binder * ty option


(** {1 Elaboration} *)

type context =
  (string * Core.ty) list

(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of loc * string

(** Raises an {!Error} exception *)
let error (loc : loc) (message : string) : 'a =
  raise (Error (loc, message))

(** {2 Bidirectional elaboration} *)

(** Validate that a type is well-formed. *)
let rec elab_ty (ty : ty) : Core.ty =
  match ty.data with
  | Name "Text" -> Text
  | Name "Bool" -> Bool
  | Name "Int" -> Int
  | Name name ->
      error ty.loc (Format.asprintf "unbound type `%s`" name)
  | Fun (ty1, ty2) ->
      Fun (elab_ty ty1, elab_ty ty2)

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  match tm.data with
  | IfThenElse (tm1, tm2, tm3) ->
      BoolElim (
        elab_check context tm1 Bool,
        elab_check context tm2 ty,
        elab_check context tm3 ty)
  | _ ->
      let tm', ty' = elab_synth context tm in
      if ty = ty' then tm' else
        error tm.loc "type mismatch"

(** Elaborate a surface term into a core term, synthesising its type. *)
and elab_synth (context : context) (tm : tm) : Core.tm * Core.ty =
  match tm.data with
  | Name name -> Var name, List.assoc name context
  | Let (name, params, def_body_ty, def_tm, body_tm) ->
      let def_tm, def_ty = elab_synth_fun_lit context params def_body_ty def_tm in
      let body_tm, body_ty = elab_synth ((name.data, def_ty) :: context) body_tm in
      Let (name.data, def_ty, def_tm, body_tm), body_ty
  | Ann (tm, ty) ->
      let ty = elab_ty ty in
      elab_check context tm ty, ty
  | Template template -> elab_template context template, Text
  | Add (tm1, tm2) ->
      TextConcat (elab_check context tm1 Text, elab_check context tm2 Text), Text
  | IfThenElse (_, _, _) ->
      error tm.loc "ambiguous if expression"
  | App (head_tm, arg_tm) -> begin
      match elab_synth context head_tm with
      | head_tm, Fun (param_ty, body_ty) ->
          let arg_tm = elab_check context arg_tm param_ty in
          FunApp (head_tm, arg_tm), body_ty
      | _ ->
          error head_tm.loc "function expected"
  end
  | TextLit s -> TextLit s, Text
  | IntLit n -> IntLit n, Int

(** Elaborate a function literal, inferring its type. *)
and elab_synth_fun_lit (context : context) (params : (binder * ty option) list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
  match params, body_ty with
  | [], Some body_ty ->
      let body_ty = elab_ty body_ty in
      elab_check context body body_ty, body_ty
  | [], None ->
      elab_synth context body
  | (name, param_ty) :: params, body_ty ->
      let param_ty = match param_ty with
        | None -> error name.loc "type annotations required in function parameters"
        | Some ty -> elab_ty ty
      in
      let body, body_ty = elab_synth_fun_lit ((name.data, param_ty) :: context) params body_ty body in
      FunLit (name.data, param_ty, body), Fun (param_ty, body_ty)

(** Elaborate a template into a series of concatened terms. *)
and[@tail_mod_cons] elab_template (context : context) (template : template) : Core.tm =
  match template with
  | [] -> TextLit ""
  | { data = TextFragment s; _ } :: template ->
      TextConcat (TextLit s, elab_template context template)
  | { data = TermFragment tm; _ } :: template ->
      let tm = elab_check context tm Text in
      TextConcat (tm, elab_template context template)
  | { data = LetFragment (name, params, def_body_ty, def_tm); _ } :: template ->
      let def_tm, def_ty = elab_synth_fun_lit context params def_body_ty def_tm in
      Let (name.data, def_ty, def_tm,
        elab_template ((name.data, def_ty) :: context) template)
  | { data = IfThenElseFragment (tm, template1, template2); _ } :: template ->
      let tm = elab_check context tm Bool in
      let template1 = elab_template context template1 in
      let template2 = elab_template context template2 in
      TextConcat (BoolElim (tm, template1, template2),
        elab_template context template)
