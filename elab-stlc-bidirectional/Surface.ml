(** {0 Surface language}

    The surface language closely mirrors what the programmer originaly wrote,
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
  | FunType of ty * ty

(** Names that bind definitions or parameters *)
type binder = string located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | FunLit of param list * tm
  | BoolLit of bool
  | IntLit of int
  | App of tm * tm
  | IfThenElse of tm * tm * tm
  | Op2 of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** {1 Elaboration} *)

(** This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)


(** {2 Local bindings} *)

(** A stack of bindings currently in scope *)
type context = (string * Core.ty) Core.env

(** Lookup a name in the context *)
let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
  ctx |> List.find_mapi @@ fun index (name', ty) ->
    match name = name' with
    | true -> Some (index, ty)
    | false -> None


(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of loc * string

(** Raises an {!Error} exception *)
let error (loc : loc) (message : string) : 'a =
  raise (Error (loc, message))

let equate_ty (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
  if ty1 = ty2 then () else
    error loc
      (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
        Core.pp_ty ty1
        Core.pp_ty ty2)


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors. We can also extend the type system with
    advanced features like dependent types, higher rank types, and subtyping
    while maintaining decidability by allowing the programmer to supply
    annotations where necessary. *)

(** Elaborate a type, checking that it is well-formed. *)
let rec elab_ty (ty : ty) : Core.ty =
  match ty.data with
  | Name "Bool" -> BoolType
  | Name "Int" -> IntType
  | Name name ->
      error ty.loc (Format.asprintf "unbound type `%s`" name)
  | FunType (ty1, ty2) ->
      FunType (elab_ty ty1, elab_ty ty2)

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
  match tm.data with
  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body = elab_check ((def_name.data, def_ty) :: ctx) body ty in
      Let (def_name.data, def_ty, def, body)

  | FunLit (params, body) ->
      elab_check_fun_lit ctx params body ty

  | IfThenElse (head, tm0, tm1) ->
      let head = elab_check ctx head BoolType in
      let tm0 = elab_check ctx tm0 ty in
      let tm1 = elab_check ctx tm1 ty in
      BoolElim (head, tm0, tm1)

  (* Fall back to type inference *)
  | _ ->
      let tm', ty' = elab_infer ctx tm in
      equate_ty tm.loc ty ty';
      tm'

(** Elaborate a surface term into a core term, inferring its type. *)
and elab_infer (ctx : context) (tm : tm) : Core.tm * Core.ty =
  match tm.data with
  | Name name -> begin
      match lookup ctx name with
      | Some (index, ty) -> Var index, ty
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
  end

  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: ctx) body in
      Let (def_name.data, def_ty, def, body), body_ty

  | Ann (tm, ty) ->
      let ty = elab_ty ty in
      elab_check ctx tm ty, ty

  | BoolLit b ->
      BoolLit b, BoolType

  | IntLit i ->
      IntLit i, IntType

  | FunLit (params, body) ->
      elab_infer_fun_lit ctx params None body

  | App (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer ctx head in
      let param_ty, body_ty =
        match head_ty with
        | FunType (param_ty, body_ty) -> param_ty, body_ty
        | head_ty ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %a@]@]"
                Core.pp_ty head_ty)
      in
      let arg = elab_check ctx arg param_ty in
      FunApp (head, arg), body_ty

  | IfThenElse (_, _, _) ->
      error tm.loc "ambiguous if expression"

  | Op2 ((`Eq) as prim, tm0, tm1) ->
      let tm0 = elab_check ctx tm0 IntType in
      let tm1 = elab_check ctx tm1 IntType in
      PrimApp (prim, [tm0; tm1]), BoolType

  | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
      let tm0 = elab_check ctx tm0 IntType in
      let tm1 = elab_check ctx tm1 IntType in
      PrimApp (prim, [tm0; tm1]), IntType

  | Op1 ((`Neg) as prim, tm) ->
      let tm = elab_check ctx tm IntType in
      PrimApp (prim, [tm]), IntType

(** Elaborate a function literal into a core term, given an expected type. *)
and elab_check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
  match params, ty with
  | [], ty ->
      elab_check ctx body ty
  | (name, None) :: params, FunType (param_ty, body_ty) ->
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      FunLit (name.data, param_ty, body)
  | (name, Some param_ty) :: params, FunType (param_ty', body_ty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty param_ty in
      equate_ty param_ty_loc param_ty param_ty';
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      FunLit (name.data, param_ty, body)
  | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

(** Elaborate a function literal into a core term, inferring its type. *)
and elab_infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
  match params, body_ty with
  | [], Some body_ty ->
      let body_ty = elab_ty body_ty in
      elab_check ctx body body_ty, body_ty
  | [], None ->
      elab_infer ctx body
  | (name, None) :: _, _ ->
      error name.loc "ambiguous parameter type"
  | (name, Some param_ty) :: params, body_ty ->
      let param_ty = elab_ty param_ty in
      let body, body_ty = elab_infer_fun_lit ((name.data, param_ty) :: ctx) params body_ty body in
      FunLit (name.data, param_ty, body), FunType (param_ty, body_ty)
