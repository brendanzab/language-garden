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
  | Placeholder

(** Names that bind definitions or parameters *)
type binder = string option located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | Bool_lit of bool
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

  type error = {
    loc : loc;
    message : string;
    details : string list;
  }


  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    tys : (string option * Core.ty) Core.env;
    (** A stack of bindings currently in scope *)

    metas : (loc * string * Core.meta) Dynarray.t;
    (** A list of the metavariables that have been inserted during elaboration.
        This will be used to generate a list of unsolved metavariables once
        elaboration is complete. *)

    errors : error Dynarray.t;
    (** Error messages recorded during elaboration.

        Alternatively we could have decided to store a error handler function,
        or to use use OCaml’s effects and handlers fir this, but this approach
        was chosen for its simplicity and directness.
    *)
  }

  (** The empty context *)
  let empty () : context = {
    tys = [];
    metas = Dynarray.create ();
    errors = Dynarray.create ();
  }

  (** Extend the context with a new binding *)
  let extend (ctx : context) (name : string option) (ty : Core.ty) : context = {
    ctx with
    tys = (name, ty) :: ctx.tys;
  }

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx.tys |> List.find_mapi @@ fun index (name', ty) ->
      match Some name = name' with
      | true -> Some (index, ty)
      | false -> None

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (loc: loc) (info : string) : Core.ty =
    let m = Core.fresh_meta () in
    Dynarray.add_last ctx.metas (loc, info, m);
    Meta_var m

  (** Record and error in the elaboration context *)
  let record_error (ctx : context) (error : error) =
    Dynarray.add_last ctx.errors error

  let unify_tys (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) : (unit, error) result =
    try Core.unify_tys ty1 ty2; Ok () with
    | Core.Infinite_type _ ->
        Error { loc; message = "infinite type"; details = [] }
    | Core.Mismatched_types (_, _) ->
        Error {
          loc;
          message = "mismatched types";
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


  (** First, we implement some wrappers around the {!record_error} function.
      These allow us to embed a failing type or term in other terms or types by
      returning error sentinels.  *)

  let check_ty_error (ctx : context) (error : error) : Core.ty =
    record_error ctx error;
    Reported_error

  let check_tm_error (ctx : context) (error : error) : Core.tm =
    record_error ctx error;
    Reported_error

  let synth_tm_error (ctx : context) (error : error) : Core.tm * Core.ty =
    record_error ctx error;
    Reported_error, Reported_error


  (** Now we implement the main bidirectional typing algorithm *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ctx : context) (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        check_ty_error ctx {
          loc = ty.loc;
          message = Format.asprintf "unbound type `%s`" name;
          details = [];
        }
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Placeholder ->
        fresh_meta ctx ty.loc "placeholder"

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
        begin match unify_tys tm.loc ty ty' with
        | Ok () -> tm'
        | Error error -> check_tm_error ctx error
        end

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (_, Reported_error) -> Reported_error, Reported_error
        | Some (index, ty) -> Var index, ty
        | None ->
            synth_tm_error ctx {
              loc = tm.loc;
              message = Format.asprintf "unbound name `%s`" name;
              details = [];
            }
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

    | Bool_lit b ->
        Bool_lit b, Bool_type

    | App (head, arg) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in

        begin match Core.force_ty head_ty with
        | Reported_error ->
            Reported_error, Reported_error
        | Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | Meta_var _ as head_ty ->
            let param_ty = fresh_meta ctx head_loc "function parameter type" in
            let body_ty = fresh_meta ctx head_loc "function return type" in
            begin match unify_tys head_loc (Fun_type (param_ty, body_ty)) head_ty with
            | Ok () -> Fun_app (head, check_tm ctx arg param_ty), body_ty
            | Error error -> synth_tm_error ctx error
            end
        | head_ty ->
            synth_tm_error ctx {
              loc = head_loc;
              message = "mismatched types";
              details = [
                Format.asprintf "@[<v>@[expected: function@]@ @[   found: %t@]@]"
                  (Core.pp_ty head_ty);
              ];
            }
        end

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let ty = fresh_meta ctx tm.loc "if expression branches" in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2), ty

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, ty0 = infer_tm ctx tm0 in
        let tm1, ty1 = infer_tm ctx tm1 in
        begin match unify_tys tm.loc ty0 ty1, Core.force_ty ty0 with
        | Ok (), Reported_error -> Reported_error, Reported_error
        | Ok (), Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Ok (), Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | Ok (), ty ->
            synth_tm_error ctx {
              loc = tm.loc;
              message = Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty);
              details = [];
            }
        | Error error, _ -> synth_tm_error ctx error
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
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (body_ty : Core.ty) : Core.tm =
    match params, Core.force_ty body_ty with
    | _, Reported_error ->
        Reported_error
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        Fun_lit (name.data, param_ty,
          check_fun_lit (extend ctx name.data param_ty) params body body_ty)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_loc = param_ty.loc in
        let param_ty = check_ty ctx param_ty in
        let param_ty =
          match unify_tys param_ty_loc param_ty param_ty' with
          | Ok () -> param_ty
          | Error error -> check_ty_error ctx error
        in
        Fun_lit (name.data, param_ty,
          check_fun_lit (extend ctx name.data param_ty) params body body_ty)
    | (name, _) :: _, Meta_var _ ->
        let body', body_ty' = infer_fun_lit ctx params None body in
        begin match unify_tys name.loc body_ty body_ty' with
        | Ok () -> body'
        | Error error -> check_tm_error ctx error
        end
    | (name, _) :: params, body_ty ->
        record_error ctx {
          loc = name.loc;
          message = "unexpected parameter";
          details = [];
        };
        check_fun_lit ctx params body body_ty


  (** Elaborate a function literal, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.loc "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : context -> a) : (a, error list) result =
    let ctx = empty () in
    let result = prog ctx in

    (* Only record ambiguity errors if no other errors have been seen during
        elaboration. This ensures that we don’t report ambiguities errors about
        erroneous code. *)
    if Dynarray.is_empty ctx.errors then begin
      ctx.metas |> Dynarray.iter begin fun (loc, info, m) ->
        match !m with
        | Core.Unsolved _ ->
            record_error ctx {
              loc;
              message = "ambiguous " ^ info;
              details = [];
            }
        | Core.Solved _ -> ()
      end;
    end;

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
