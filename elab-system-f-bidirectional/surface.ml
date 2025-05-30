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

(** Names that bind definitions or parameters *)
type binder = string option located

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | Forall_type of binder list * ty
  | Fun_type of ty * ty

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Bool_lit of bool
  | Int_lit of int
  | App of tm * arg
  | If_then_else of tm * tm * tm
  | Op2 of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  | Ty_param of binder
  | Param of binder * ty option

and arg =
  | Ty_arg of ty
  | Arg of tm


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab = struct

  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    size : Core.level;
    names : string option Core.env;
    decls : Core.Semantics.decl Core.env;
    defns : Core.Semantics.defn Core.env;
  }

  (** The empty context *)
  let empty : context = {
    size = 0;
    names = [];
    decls = [];
    defns = [];
  }

  (** The type variable that will be bound after calling {!extend_ty} *)
  let next_ty_var (ctx : context) : Core.Semantics.vty =
    Var ctx.size

  (** Extend the context with a type binding *)
  let extend_ty (ctx : context) (name : string option) : context = {
    size = ctx.size + 1;
    names = name :: ctx.names;
    decls = Ty_decl :: ctx.decls;
    defns = Ty_defn (next_ty_var ctx) :: ctx.defns;
  }

  (** Extend the context with a term binding *)
  let extend_tm (ctx : context) (name : string option) (vty : Core.Semantics.vty) : context = {
    size = ctx.size + 1;
    names = name :: ctx.names;
    decls = Tm_decl vty :: ctx.decls;
    defns = Tm_defn (Neu (Var ctx.size)) :: ctx.defns;
  }

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.Semantics.decl) option =
    ctx.names |> List.find_mapi @@ fun index name' ->
      match Some name = name' with
      | true -> Some (index, List.nth ctx.decls index)
      | false -> None

  let eval_ty (ctx : context) (ty : Core.ty) : Core.Semantics.vty =
    Core.Semantics.eval_ty ctx.defns ty

  let quote_vty (ctx : context) (vty : Core.Semantics.vty) : Core.ty =
    Core.Semantics.quote_vty ctx.size vty

  let pp_ty (ctx : context) (fmt : Format.formatter) (ty : Core.ty) : unit =
    Core.pp_ty ctx.names fmt ty


  (** {2 Elaboration errors} *)

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of loc * string

  (** Raises an {!Error} exception *)
  let error (type a) (loc : loc) (message : string) : a =
    raise (Error (loc, message))

  let equate_vtys (ctx : context) (loc : loc) (vty1 : Core.Semantics.vty) (vty2 : Core.Semantics.vty) =
    if Core.Semantics.is_convertible ctx.size vty1 vty2 then () else
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          (pp_ty ctx) (quote_vty ctx vty1)
          (pp_ty ctx) (quote_vty ctx vty2))


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
    | Name name ->
        begin match lookup ctx name with
        | Some (index, Ty_decl) -> Var index
        | Some (_, Tm_decl _) -> error ty.loc (Format.asprintf "expected type, found `%s`" name)
        | None when name = "Bool" -> Bool_type
        | None when name = "Int" -> Int_type
        | None -> error ty.loc (Format.asprintf "unbound type `%s`" name)
        end
    | Forall_type (names, body_ty) ->
        let rec go ctx names : Core.ty =
          match names with
          | [] -> check_ty ctx body_ty
          | name :: names -> Forall_type (name.data, go (extend_ty ctx name.data) names)
        in
        go ctx names
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ctx ty1, check_ty ctx ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (vty : Core.Semantics.vty) : Core.tm =
    match tm.data with
    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend_tm ctx def_name.data def_vty) body vty in
        Let (def_name.data, quote_vty ctx def_vty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body vty

    | If_then_else (head, tm0, tm1) ->
        let head = check_tm ctx head Bool_type in
        let tm0 = check_tm ctx tm0 vty in
        let tm1 = check_tm ctx tm1 vty in
        Bool_elim (head, tm0, tm1)

    (* Fall back to type inference *)
    | _ ->
        let tm', vty' = infer_tm ctx tm in
        equate_vtys ctx tm.loc vty vty';
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.Semantics.vty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (_, Ty_decl) -> error tm.loc (Format.asprintf "expected term, found `%s`" name)
        | Some (index, Tm_decl vty) -> Var index, vty
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend_tm ctx def_name.data def_vty) body in
        Let (def_name.data, quote_vty ctx def_vty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        let vty = eval_ty ctx ty in
        check_tm ctx tm vty, vty

    | Bool_lit b ->
        Bool_lit b, Bool_type

    | Int_lit i ->
        Int_lit i, Int_type

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | App (head, Ty_arg arg) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in
        let body_ty =
          match head_ty with
          | Forall_type (_, body_ty) -> body_ty
          | head_vty ->
              error head_loc
                (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: type function@]@ @[found: %a@]@]"
                  (pp_ty ctx) (quote_vty ctx head_vty))
        in
        let arg = check_ty ctx arg in
        Forall_app (head, arg), body_ty (eval_ty ctx arg)

    | App (head, Arg arg) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in
        let param_ty, body_ty =
          match head_ty with
          | Fun_type (param_ty, body_ty) -> param_ty, body_ty
          | head_vty ->
              error head_loc
                (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %a@]@]"
                  (pp_ty ctx) (quote_vty ctx head_vty))
        in
        let arg = check_tm ctx arg param_ty in
        Fun_app (head, arg), body_ty

    | If_then_else (_, _, _) ->
        error tm.loc "ambiguous if expression"

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, vty0 = infer_tm ctx tm0 in
        let tm1, vty1 = infer_tm ctx tm1 in
        equate_vtys ctx tm.loc vty0 vty1;
        begin match vty0 with
        | Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | vty -> error tm.loc (Format.asprintf "@[unsupported type: %a@]" (pp_ty ctx) (quote_vty ctx vty))
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
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (vty : Core.Semantics.vty) : Core.tm =
    match params, vty with
    | [], vty ->
        check_tm ctx body vty

    | Ty_param name :: params, Forall_type (_, body_vty) ->
        let ty_var = next_ty_var ctx in
        let body_tm = check_fun_lit (extend_ty ctx name.data) params body (body_vty ty_var) in
        Forall_lit (name.data, body_tm)

    (* Insert missing type parameters *)
    | params, Forall_type (name, body_vty) ->
        let ty_var = next_ty_var ctx in
        let name = name |> Option.map (fun n -> "$" ^ n) in
        let body_tm = check_fun_lit (extend_ty ctx name) params body (body_vty ty_var) in
        Forall_lit (name, body_tm)

    | Param (name, None) :: params, Fun_type (param_vty, body_vty) ->
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty in
        Fun_lit (name.data, quote_vty ctx param_vty, body_tm)

    | Param (name, Some param_ty) :: params, Fun_type (param_vty', body_vty) ->
        let param_ty_loc = param_ty.loc in
        let param_ty = check_ty ctx param_ty in
        let param_vty = eval_ty ctx param_ty in
        equate_vtys ctx param_ty_loc param_vty param_vty';
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty in
        Fun_lit (name.data, param_ty, body_tm)

    | Ty_param name :: _, _ ->
        error name.loc "unexpected type parameter"

    | Param (name, _) :: _, _ ->
        error name.loc "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.Semantics.vty =
    let rec go ctx params body_ty body =
      match params, body_ty with
      | [], Some body_ty ->
          let body_ty = check_ty ctx body_ty in
          let body_vty = eval_ty ctx body_ty in
          check_tm ctx body body_vty, body_ty

      | [], None ->
          let body, body_vty = infer_tm ctx body in
          body, quote_vty ctx body_vty

      | Ty_param name :: params, body_ty ->
          let body, body_ty = go (extend_ty ctx name.data) params body_ty body in
          Forall_lit (name.data, body), Forall_type (name.data, body_ty)

      | Param (name, None) :: _, _ ->
          error name.loc "ambiguous parameter type"

      | Param (name, Some param_ty) :: params, body_ty ->
          let param_ty = check_ty ctx param_ty in
          let body, body_ty =
            let ctx = extend_tm ctx name.data (eval_ty ctx param_ty) in
            infer_fun_lit ctx params body_ty body in
          Fun_lit (name.data, param_ty, body), Fun_type (param_ty, quote_vty ctx body_ty)
    in

    let body, body_ty = go ctx params body_ty body in
    body, eval_ty ctx body_ty

end
