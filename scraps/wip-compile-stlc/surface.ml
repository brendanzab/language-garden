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
  | Fun_ty of ty * ty

(** Names that bind definitions or parameters *)
type binder = string located

(** Terms in the surface language *)
type expr =
  expr_data located

and expr_data =
  | Name of string
  | Let of binder * param list * ty option * expr * expr
  | Ann of expr * ty
  | Fun_lit of param list * expr
  | Bool_lit of bool
  | Int_lit of int
  | App of expr * expr
  | If_then_else of expr * expr * expr
  | Infix of [`Eq | `Add | `Sub | `Mul | `Or | `And] * expr * expr
  | Prefix of [`Neg | `Not] * expr

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
type context = (string * Core.ty) list

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
let error (type a) (loc : loc) (message : string) : a =
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
  | Name "Bool" -> Bool_ty
  | Name "Int" -> Int_ty
  | Name name ->
      error ty.loc (Format.asprintf "unbound type `%s`" name)
  | Fun_ty (ty1, ty2) ->
      Fun_ty (elab_ty ty1, elab_ty ty2)

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (ctx : context) (expr : expr) (ty : Core.ty) : Core.expr =
  match expr.data with
  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body = elab_check ((def_name.data, def_ty) :: ctx) body ty in
      Let (User def_name.data, def_ty, def, body)

  | Fun_lit (params, body) ->
      elab_check_fun_lit ctx params body ty

  | If_then_else (head, expr1, expr2) ->
      let head = elab_check ctx head Bool_ty in
      let expr1 = elab_check ctx expr1 ty in
      let expr2 = elab_check ctx expr2 ty in
      Bool_elim (head, expr1, expr2)

  (* Fall back to type inference *)
  | _ ->
      let expr', ty' = elab_infer ctx expr in
      equate_ty expr.loc ty ty';
      expr'

(** Elaborate a surface term into a core term, inferring its type. *)
and elab_infer (ctx : context) (expr : expr) : Core.expr * Core.ty =
  match expr.data with
  | Name name -> begin
      match lookup ctx name with
      | Some (index, ty) -> Var index, ty
      | None -> error expr.loc (Format.asprintf "unbound name `%s`" name)
  end

  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: ctx) body in
      Let (User def_name.data, def_ty, def, body), body_ty

  | Ann (expr, ty) ->
      let ty = elab_ty ty in
      elab_check ctx expr ty, ty

  | Bool_lit b ->
      Bool_lit b, Bool_ty

  | Int_lit i ->
      Int_lit i, Int_ty

  | Fun_lit (params, body) ->
      elab_infer_fun_lit ctx params None body

  | App (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer ctx head in
      let param_ty, body_ty =
        match head_ty with
        | Fun_ty (param_ty, body_ty) -> param_ty, body_ty
        | head_ty ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %a@]@]"
                Core.pp_ty head_ty)
      in
      let arg = elab_check ctx arg param_ty in
      Fun_app (head, arg), body_ty

  | If_then_else (_, _, _) ->
      error expr.loc "ambiguous if expression"

  | Infix (`Eq, expr1, expr2) ->
      let expr1, ty1 = elab_infer ctx expr1 in
      let expr2, ty2 = elab_infer ctx expr2 in
      equate_ty expr.loc ty1 ty2;
      begin match ty1 with
      | Bool_ty -> Fun_app (Prim Bool_eq, Tuple_lit [expr1; expr2]), Bool_ty
      | Int_ty -> Fun_app (Prim Int_eq, Tuple_lit [expr1; expr2]), Bool_ty
      | ty -> error expr.loc (Format.asprintf "@[unsupported type: %a@]" Core.pp_ty ty)
      end

  | Infix ((`Add | `Sub | `Mul) as prim, expr1, expr2) ->
      let prim =
        match prim with
        | `Add -> Prim.Int_add
        | `Sub -> Prim.Int_sub
        | `Mul -> Prim.Int_mul
      in
      let expr1 = elab_check ctx expr1 Int_ty in
      let expr2 = elab_check ctx expr2 Int_ty in
      Fun_app (Prim prim, Tuple_lit [expr1; expr2]), Int_ty

  | Infix (`And, expr1, expr2) ->
      let expr1 = elab_check ctx expr1 Bool_ty in
      let expr2 = elab_check ctx expr2 Bool_ty in
      Bool_elim (expr1, expr2, Bool_lit false), Bool_ty

  | Infix (`Or, expr1, expr2) ->
      let expr1 = elab_check ctx expr1 Bool_ty in
      let expr2 = elab_check ctx expr2 Bool_ty in
      Bool_elim (expr1, Bool_lit true, expr2), Bool_ty

  | Prefix (`Neg, expr) ->
      let expr = elab_check ctx expr Int_ty in
      Fun_app (Prim Int_neg, Tuple_lit [expr]), Int_ty

  | Prefix (`Not, expr) ->
      let expr = elab_check ctx expr Bool_ty in
      Fun_app (Prim Bool_not, Tuple_lit [expr]), Bool_ty


(** Elaborate a function literal into a core term, given an expected type. *)
and elab_check_fun_lit (ctx : context) (params : param list) (body : expr) (ty : Core.ty) : Core.expr =
  match params, ty with
  | [], ty ->
      elab_check ctx body ty
  | (name, None) :: params, Fun_ty (param_ty, body_ty) ->
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      Fun_lit (User name.data, param_ty, body)
  | (name, Some param_ty) :: params, Fun_ty (param_ty', body_ty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty param_ty in
      equate_ty param_ty_loc param_ty param_ty';
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      Fun_lit (User name.data, param_ty, body)
  | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

(** Elaborate a function literal into a core term, inferring its type. *)
and elab_infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : expr) : Core.expr * Core.ty =
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
      Fun_lit (User name.data, param_ty, body), Fun_ty (param_ty, body_ty)
