(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

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
  | Fun_type of ty * ty

(** Names that bind definitions or parameters *)
type binder = string spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * tm
  | If_then_else of tm * tm * tm
  | Infix of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** {1 User-facing diagnostics} *)

(** An error message that should be reported to the programmer *)
module Error = struct

  type t = {
    span : span;
    message : string;
    details : string list;
  }

  let make ?(details = ([] : string list)) (span : span) (message : string) : t =
    { span; message; details }

end


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : ty -> (Core.ty, Error.t list) result
  val check_tm : tm -> Core.ty -> (Core.tm, Error.t list) result
  val infer_tm : tm -> (Core.tm * Core.ty, Error.t list) result

end = struct

  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    tys : (string * Core.ty) Core.env;
    (** A stack of bindings currently in scope *)

    errors : Error.t Dynarray.t;
    (** Error messages recorded during elaboration. *)
  }

  (** The empty context *)
  let empty () : context = {
    tys = [];
    errors = Dynarray.create ();
  }

  (** Extend the context with a new binding *)
  let extend (ctx : context) (name : string) (ty : Core.ty) : context = {
    ctx with
    tys = (name, ty) :: ctx.tys;
  }

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx.tys |> List.find_mapi @@ fun index (name', ty) ->
      match name = name' with
      | true -> Some (index, ty)
      | false -> None

  (** Record an error in the elaboration context *)
  let report (ctx : context) (error : Error.t) =
    Dynarray.add_last ctx.errors error

  let error (type a) ?(details : string list option) (ctx : context) (span : span) : (a, Format.formatter, unit, unit) format4 -> a =
    Format.kasprintf @@ fun message ->
      report ctx (Error.make span message ?details)

  (** Check if two types are compatible with each other. *)
  let equate_tys (span : span) ~(found : Core.ty) ~(expected : Core.ty) : (unit, Error.t) result =
    if Core.equate_tys found expected then Ok () else
      Result.error @@ Error.make span "mismatched types"
        ~details:[
          Format.asprintf "@[<v>@[expected: %t@]@ @[   found: %t@]@]"
            (Core.pp_ty expected)
            (Core.pp_ty found);
        ]


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ctx : context) (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Core.Bool_type
    | Name "Int" -> Core.Int_type
    | Name name ->
        error ctx ty.span "unbound type `%s`" name;
        Core.Unknown_type
    | Fun_type (ty1, ty2) ->
        Core.Fun_type (check_ty ctx ty1, check_ty ctx ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend ctx def_name.data def_ty) body ty in
        Core.Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        begin match equate_tys tm.span ~found:ty' ~expected:ty with
        | Ok () -> tm'
        | Error error ->
            report ctx error;
            Core.Reported_error
        end

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, ty) -> Core.Var index, ty
        | None when name = "true" -> Core.Bool_lit true, Core.Bool_type
        | None when name = "false" -> Core.Bool_lit false, Core.Bool_type
        | None ->
            error ctx tm.span "unbound name `%s`" name;
            Core.Reported_error, Core.Unknown_type
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend ctx def_name.data def_ty) body in
        Core.Let (def_name.data, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | Int_lit i ->
        Core.Int_lit i, Core.Int_type

    | App (head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match head_ty with
        | Core.Unknown_type ->
            Core.Reported_error, Core.Unknown_type
        | Core.Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty
        | _ ->
            error ctx arg.span "unexpected argument";
            Core.Reported_error, Core.Unknown_type
        end

    | If_then_else (head, tm1, ({ span = tm2_span; _ } as tm2)) ->
        let head = check_tm ctx head Core.Bool_type in
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        begin match Core.meet_tys ty1 ty2 with
        | Some ty -> Bool_elim (head, tm1, tm2), ty
        | None ->
            error ctx tm2_span "mismatched branches of if expression"
              ~details:[
                Format.asprintf "@[<v>@[expected: %t@]@ @[   found: %t@]@]"
                  (Core.pp_ty ty1)
                  (Core.pp_ty ty2);
              ];
            Core.Reported_error, Core.Unknown_type
        end

    | Infix (`Eq, tm1, ({ span = tm2_span; _ } as tm2)) ->
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        begin match Core.meet_tys ty1 ty2 with
        | Some Core.Unknown_type -> Core.Reported_error, Core.Unknown_type
        | Some Core.Bool_type -> Core.Prim_app (Prim.Bool_eq, [tm1; tm2]), Core.Bool_type
        | Some Core.Int_type -> Core.Prim_app (Prim.Int_eq, [tm1; tm2]), Core.Bool_type
        | Some ty ->
            error ctx tm.span
              "@[<h>cannot compare operands of type `%t`@]"
              (Core.pp_ty ty)
              ~details:[
                Format.asprintf "expected `%t` or `%t`"
                  (Core.pp_ty Core.Bool_type)
                  (Core.pp_ty Core.Int_type);
              ];
            Core.Reported_error, Core.Unknown_type
        | None ->
            error ctx tm2_span "mismatched operands"
              ~details:[
                Format.asprintf "@[<v>@[expected: %t@]@ @[   found: %t@]@]"
                  (Core.pp_ty ty1)
                  (Core.pp_ty ty2);
              ];
            Core.Reported_error, Core.Unknown_type
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Core.Int_type in
        let tm2 = check_tm ctx tm2 Core.Int_type in
        Core.Prim_app (prim, [tm1; tm2]), Core.Int_type

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Int_type in
        Core.Prim_app (Prim.Int_neg, [tm]), Core.Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    (* Extract the parameter type and body type, if possible, propagating unknown
       types as required. See section 2.1.4 of “Total Type Error Localization
       and Recovery with Holes” by Zhao et. al. for more details. *)
    let match_fun_ty (ty : Core.ty) =
      match ty with
      | Fun_type (param_ty, body_ty) -> Some (param_ty, body_ty)
      | Core.Unknown_type -> Some (Core.Unknown_type, Core.Unknown_type)
      | _ -> None
    in

    match params, match_fun_ty ty with
    | [], _ ->
        check_tm ctx body ty

    | (name, None) :: params, Some (param_ty, body_ty) ->
        Fun_lit (name.data, param_ty,
          check_fun_lit (extend ctx name.data param_ty) params body body_ty)

    | (name, Some ({ span = ann_span ; _ } as param_ty)) :: params, Some (param_ty', body_ty) ->
        let param_ty = check_ty ctx param_ty in
        begin match equate_tys ann_span ~found:param_ty ~expected:param_ty' with
        | Ok () ->
            Fun_lit (name.data, param_ty,
              check_fun_lit (extend ctx name.data param_ty) params body body_ty)
        (* The explicit parameter did not match the expected type. Continue
           checking the body of the function regardless. *)
        | Error error ->
            report ctx error;
            let _ = check_fun_lit (extend ctx name.data param_ty) params body body_ty in
            Core.Reported_error
        end

    (* If we see an unexpected parameter, we check the parameter type regardless
       and continue checking the body of the function. *)
    | (name, param_ty) :: params, None ->
        error ctx name.span "unexpected parameter";
        let param_ty = match param_ty with
          | Some param_ty -> check_ty ctx param_ty
          | None -> Core.Unknown_type
        in
        let _ = check_fun_lit (extend ctx name.data param_ty) params body Core.Unknown_type in
        Core.Reported_error

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, param_ty) :: params, body_ty ->
        let param_ty =
          match param_ty with
          | Some ty -> check_ty ctx ty
          | None ->
              error ctx name.span "ambiguous parameter type";
              Core.Unknown_type
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_type (param_ty, body_ty)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : context -> a) : (a, Error.t list) result =
    let ctx = empty () in
    let result = prog ctx in

    match Dynarray.to_list ctx.errors with
    | [] -> Ok result
    | errors -> Error errors


  (** {2 Public API} *)

  let check_ty (ty : ty) : (Core.ty, Error.t list) result =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (ty : Core.ty) : (Core.tm, Error.t list) result =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty, Error.t list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
