(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

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
  | Fun_type of ty * ty

(** Names that bind definitions or parameters *)
type binder = string located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * tm
  | If_then_else of tm * tm * tm
  | Op2 of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  type error = {
    loc : loc;
    message : string;
    details : string list;
  }

  val check_ty : ty -> (Core.ty, error list) result
  val check_tm : tm -> Core.ty -> (Core.tm, error list) result
  val infer_tm : tm -> (Core.tm * Core.ty, error list) result

end = struct

  (** {2 Elaboration errors} *)

  type error = {
    loc : loc;
    message : string;
    details : string list;
  }

  let error (loc : loc) (message : string) : error =
    { loc; message; details = [] }


  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    tys : (string * Core.ty) Core.env;
    (** A stack of bindings currently in scope *)

    errors : error Dynarray.t;
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
  let report (ctx : context) (error : error) =
    Dynarray.add_last ctx.errors error

  let equate_ty (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) : (unit, error) result =
    match ty1, ty2 with
    | Reported_error, _ | _, Reported_error -> Ok ()
    | ty1, ty2 when ty1 = ty2 -> Ok ()
    | ty1, ty2 ->
        Error {
          (error loc "mismatched types") with
          details = [
            Format.asprintf "@[<v>@[expected: %t@]@ @[   found: %t@]@]"
              (Core.pp_ty ty1)
              (Core.pp_ty ty2);
          ];
        }


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
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        report ctx (error ty.loc (Format.asprintf "unbound type `%s`" name));
        Reported_error
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ctx ty1, check_ty ctx ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend ctx def_name.data def_ty) body ty in
        Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        begin match equate_ty tm.loc ty ty' with
        | Ok () -> tm'
        | Error error ->
            report ctx error;
            Reported_error
        end

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, ty) -> Var index, ty
        | None when name = "true" -> Bool_lit true, Bool_type
        | None when name = "false" -> Bool_lit false, Bool_type
        | None ->
            report ctx (error tm.loc (Format.asprintf "unbound name `%s`" name));
            Reported_error, Reported_error
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend ctx def_name.data def_ty) body in
        Let (def_name.data, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | Int_lit i ->
        Int_lit i, Int_type

    | App ({ loc = head_loc; _ } as head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match head_ty with
        | Reported_error ->
            Reported_error, Reported_error
        | Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | head_ty ->
            report ctx {
              (error head_loc "mismatched types") with
              details = [
                Format.asprintf "@[<v>@[expected: function@]@ @[   found: %t@]@]"
                  (Core.pp_ty head_ty);
              ];
            };
            Reported_error, Reported_error
        end

    | If_then_else (_, _, _) ->
        report ctx (error tm.loc "ambiguous if expression");
        Reported_error, Reported_error

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, ty0 = infer_tm ctx tm0 in
        let tm1, ty1 = infer_tm ctx tm1 in
        begin match equate_ty tm.loc ty0 ty1, ty0 with
        | Ok (), Reported_error -> Reported_error, Reported_error
        | Ok (), Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Ok (), Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | Ok (), ty ->
            report ctx (error tm.loc (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty)));
            Reported_error, Reported_error
        | Error error, _ ->
            report ctx error;
            Reported_error, Reported_error
        end

    | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm0 = check_tm ctx tm0 Int_type in
        let tm1 = check_tm ctx tm1 Int_type in
        Prim_app (prim, [tm0; tm1]), Int_type

    | Op1 (`Neg, tm) ->
        let tm = check_tm ctx tm Int_type in
        Prim_app (Int_neg, [tm]), Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, ty with
    | [], ty ->
        check_tm ctx body ty

    | (name, param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty =
          match param_ty with
          | None -> param_ty'
          | Some ({ loc; _ } as param_ty) ->
              let param_ty = check_ty ctx param_ty in
              match equate_ty loc param_ty param_ty' with
              | Ok () -> param_ty
              | Error error ->
                  report ctx error;
                  Reported_error
        in
        Fun_lit (name.data, param_ty,
          check_fun_lit (extend ctx name.data param_ty) params body body_ty)

    (* If the expected type comes from a previously reported error, continue
       checking parameters in a degraded state *)
    | (name, param_ty) :: params, Reported_error ->
        let param_ty = match param_ty with
          | Some ty -> check_ty ctx ty
          | None -> Reported_error
        in
        Fun_lit (name.data, param_ty,
          check_fun_lit (extend ctx name.data param_ty) params body Reported_error)

    | (name, param_ty) :: params, ty ->
        report ctx (error name.loc "unexpected parameter");
        param_ty |> Option.iter (fun ty -> ignore (check_ty ctx ty));
        check_fun_lit ctx params body ty

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
              report ctx (error name.loc "ambiguous parameter type");
              Reported_error
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : context -> a) : (a, error list) result =
    let ctx = empty () in
    let result = prog ctx in

    match Dynarray.to_list ctx.errors with
    | [] -> Ok result
    | errors -> Error errors


  (** {2 Public API} *)

  let check_ty (ty : ty) : (Core.ty, error list) result =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (ty : Core.ty) : (Core.tm, error list) result =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty, error list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
