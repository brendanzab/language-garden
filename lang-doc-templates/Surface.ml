(** The surface language. *)

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
  | Name of string * ty list
  | FunTy of ty * ty

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | IfThenElse of tm * tm * tm
  | TemplateLit of template
  | ListLit of tm list
  | TextLit of string
  | IntLit of int
  | App of tm * tm
  | Op2 of [`Add] * tm * tm

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


module Elab = struct
  (** Type checking and elaboration from the surface language to the core language. *)

  type context =
    (string * Core.ty) list

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of loc * string

  (** Raises an {!Error} exception *)
  let error (loc : loc) (message : string) : 'a =
    raise (Error (loc, message))


  (** {1 Bidirectional elaboration} *)

  (** Validate that a type is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name ("List", [ty]) -> ListTy (check_ty ty)
    | Name ("Text", []) -> TextTy (* TODO: improve arity errors *)
    | Name ("Bool", []) -> BoolTy (* TODO: improve arity errors *)
    | Name ("Int", []) -> IntTy (* TODO: improve arity errors *)
    | Name (name, _) ->
        error ty.loc (Format.asprintf "unbound type `%s`" name)
    | FunTy (ty1, ty2) ->
        FunTy (check_ty ty1, check_ty ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | IfThenElse (tm1, tm2, tm3) ->
        BoolElim (
          check context tm1 BoolTy,
          check context tm2 ty,
          check context tm3 ty)
    | ListLit tms ->
        check_list context tms
          (match ty with
            | ListTy ty -> ty
            | _ -> error tm.loc "unexpected list literal")
    | _ ->
        let tm', ty' = synth context tm in
        if ty = ty' then tm' else
          error tm.loc "type mismatch"

  (** Elaborate a surface term into a core term, synthesising its type. *)
  and synth (context : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        Var name, List.assoc name context
    | Let (name, params, def_body_ty, def_tm, body_tm) ->
        let def_tm, def_ty = fun_lit context params def_body_ty def_tm in
        let body_tm, body_ty = synth ((name.data, def_ty) :: context) body_tm in
        Let (name.data, def_ty, def_tm, body_tm), body_ty
    | Ann (tm, ty) ->
        let ty = check_ty ty in
        check context tm ty, ty
    | IfThenElse (_, _, _) ->
        error tm.loc "ambiguous if expression"
    | TemplateLit template ->
        synth_template context template, TextTy
    | ListLit _ ->
        error tm.loc "ambiguous list literal"
    | TextLit s ->
        TextLit s, TextTy
    | IntLit n ->
        IntLit n, IntTy
    | App (head_tm, arg_tm) -> begin
        match synth context head_tm with
        | head_tm, FunTy (param_ty, body_ty) ->
            let arg_tm = check context arg_tm param_ty in
            FunApp (head_tm, arg_tm), body_ty
        | _ ->
            error head_tm.loc "function expected"
    end
    | Op2 (`Add, tm1, tm2) ->
        let tm1 = check context tm1 TextTy in
        let tm2 = check context tm2 TextTy in
        TextConcat (tm1, tm2), TextTy

  (** Elaborate a function literal, inferring its type. *)
  and fun_lit (context : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty body_ty in
        check context body body_ty, body_ty
    | [], None ->
        synth context body
    | (name, None) :: _, _ ->
        error name.loc "type annotations required in function parameters"
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty param_ty in
        let body, body_ty = fun_lit ((name.data, param_ty) :: context) params body_ty body in
        FunLit (name.data, param_ty, body), FunTy (param_ty, body_ty)

  (** Elaborate a list literal. *)
  and[@tail_mod_cons] check_list (context : context) (tms : tm list) (elem_ty : Core.ty) : Core.tm =
    match tms with
    | [] ->
        Core.ListNil
    | tm :: tms ->
        let tm = check context tm elem_ty in
        Core.ListCons (tm, check_list context tms elem_ty)

  (** Elaborate a template into a series of concatened terms. *)
  and[@tail_mod_cons] synth_template (context : context) (template : template) : Core.tm =
    match template with
    | [] -> TextLit ""
    | { data = TextFragment s; _ } :: template ->
        TextConcat (TextLit s, synth_template context template)
    | { data = TermFragment tm; _ } :: template ->
        let tm = check context tm TextTy in
        TextConcat (tm, synth_template context template)
    | { data = LetFragment (name, params, def_body_ty, def_tm); _ } :: template ->
        let def_tm, def_ty = fun_lit context params def_body_ty def_tm in
        Let (name.data, def_ty, def_tm,
          synth_template ((name.data, def_ty) :: context) template)
    | { data = IfThenElseFragment (tm, template1, template2); _ } :: template ->
        let tm = check context tm BoolTy in
        let template1 = synth_template context template1 in
        let template2 = synth_template context template2 in
        TextConcat (BoolElim (tm, template1, template2),
          synth_template context template)

end
