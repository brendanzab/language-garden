(** The surface language. *)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder = string spanned

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string * ty list
  | Fun_ty of ty * ty

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | If_then_else of tm * tm * tm
  | Template_lit of template
  | List_lit of tm list
  | Text_lit of string
  | Int_lit of int
  | App of tm * tm
  | Infix of [`Add] * tm * tm

(** Templates *)
and template =
  fragment list

(** Template fragments *)
and fragment =
  fragment_data spanned

and fragment_data =
  | Text_fragment of string
  | Term_fragment of tm
  | Let_fragment of binder * param list * ty option * tm
  | If_then_else_fragment of tm * template * template

and param =
  binder * ty option


module Elab = struct
  (** Type checking and elaboration from the surface language to the core language. *)

  type context =
    (string * Core.ty) list

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of span * string

  (** Raises an {!Error} exception *)
  let error (type a) (span : span) (message : string) : a =
    raise (Error (span, message))


  (** {1 Bidirectional elaboration} *)

  (** Validate that a type is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name ("List", [ty]) -> Core.List_ty (check_ty ty)
    | Name ("Text", []) -> Core.Text_ty (* TODO: improve arity errors *)
    | Name ("Bool", []) -> Core.Bool_ty (* TODO: improve arity errors *)
    | Name ("Int", []) -> Core.Int_ty (* TODO: improve arity errors *)
    | Name (name, _) ->
        error ty.span (Format.asprintf "unbound type `%s`" name)
    | Fun_ty (ty1, ty2) ->
        Core.Fun_ty (check_ty ty1, check_ty ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | If_then_else (tm1, tm2, tm3) ->
        Core.Bool_elim (
          check ctx tm1 Core.Bool_ty,
          check ctx tm2 ty,
          check ctx tm3 ty)
    | List_lit tms ->
        check_list ctx tms
          (match ty with
            | Core.List_ty ty -> ty
            | _ -> error tm.span "unexpected list literal")
    | _ ->
        let tm', ty' = synth ctx tm in
        if ty = ty' then tm' else
          error tm.span "type mismatch"

  (** Elaborate a surface term into a core term, synthesising its type. *)
  and synth (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match List.assoc_opt name ctx with
        | Some ty -> Var name, ty
        | None when name = "true" -> Core.Bool_lit true, Core.Bool_ty
        | None when name = "false" -> Core.Bool_lit false, Core.Bool_ty
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end
    | Let (name, params, def_body_ty, def_tm, body_tm) ->
        let def_tm, def_ty = fun_lit ctx params def_body_ty def_tm in
        let body_tm, body_ty = synth ((name.data, def_ty) :: ctx) body_tm in
        Core.Let (name.data, def_ty, def_tm, body_tm), body_ty
    | Ann (tm, ty) ->
        let ty = check_ty ty in
        check ctx tm ty, ty
    | If_then_else (_, _, _) ->
        error tm.span "ambiguous if expression"
    | Template_lit template ->
        synth_template ctx template, Core.Text_ty
    | List_lit _ ->
        error tm.span "ambiguous list literal"
    | Text_lit s ->
        Core.Text_lit s, Core.Text_ty
    | Int_lit n ->
        Core.Int_lit n, Core.Int_ty
    | App (head_tm, arg_tm) -> begin
        match synth ctx head_tm with
        | head_tm, Core.Fun_ty (param_ty, body_ty) ->
            let arg_tm = check ctx arg_tm param_ty in
            Core.Fun_app (head_tm, arg_tm), body_ty
        | _ ->
            error head_tm.span "function expected"
    end
    | Infix (`Add, tm1, tm2) ->
        let tm1 = check ctx tm1 Core.Text_ty in
        let tm2 = check ctx tm2 Core.Text_ty in
        Core.Prim_app (Prim.Text_concat, [tm1; tm2]), Core.Text_ty

  (** Elaborate a function literal, inferring its type. *)
  and fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty body_ty in
        check ctx body body_ty, body_ty
    | [], None ->
        synth ctx body
    | (name, None) :: _, _ ->
        error name.span "type annotations required in function parameters"
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty param_ty in
        let body, body_ty = fun_lit ((name.data, param_ty) :: ctx) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_ty (param_ty, body_ty)

  (** Elaborate a list literal. *)
  and[@tail_mod_cons] check_list (ctx : context) (tms : tm list) (elem_ty : Core.ty) : Core.tm =
    match tms with
    | [] ->
        Core.List_nil
    | tm :: tms ->
        let tm = check ctx tm elem_ty in
        Core.List_cons (tm, check_list ctx tms elem_ty)

  (** Elaborate a template into a series of concatened terms. *)
  and[@tail_mod_cons] synth_template (ctx : context) (template : template) : Core.tm =
    match template with
    | [] -> Text_lit ""
    | { data = Text_fragment s; _ } :: template ->
        Core.Prim_app (Prim.Text_concat, [Core.Text_lit s; synth_template ctx template])
    | { data = Term_fragment tm; _ } :: template ->
        let tm = check ctx tm Core.Text_ty in
        Core.Prim_app (Prim.Text_concat, [tm; synth_template ctx template])
    | { data = Let_fragment (name, params, def_body_ty, def_tm); _ } :: template ->
        let def_tm, def_ty = fun_lit ctx params def_body_ty def_tm in
        Core.Let (name.data, def_ty, def_tm,
          synth_template ((name.data, def_ty) :: ctx) template)
    | { data = If_then_else_fragment (tm, template1, template2); _ } :: template ->
        let tm = check ctx tm Core.Bool_ty in
        let template1 = synth_template ctx template1 in
        let template2 = synth_template ctx template2 in
        Core.Prim_app (Prim.Text_concat, [
          Core.Bool_elim (tm, template1, template2);
          synth_template ctx template;
        ])

end
