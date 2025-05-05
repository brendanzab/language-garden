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
  | Placeholder
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


(** {1 Elaboration} *)

(** This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)


(** {2 Local bindings} *)

(** The elaboration context, that keeps track of type and term bindings in
    separate namespaces. *)
type context = {
  ty_size : int;
  ty_names : string option Core.env;
  ty_env : Core.Semantics.vty Core.env;
  tm_tys : (string option * Core.Semantics.vty) Core.env;
}

(** The empty context *)
let empty : context = {
  ty_size = 0;
  ty_names = [];
  ty_env = [];
  tm_tys = [];
}

(** The type variable that will be bound after calling {!extend_ty} *)
let next_ty_var (ctx : context) : Core.Semantics.vty =
  Neu (Local_var ctx.ty_size)

(** Extend the context with a type binding *)
let extend_ty (ctx : context) (name : string option) : context = {
  ctx with
  ty_size = ctx.ty_size + 1;
  ty_names = name :: ctx.ty_names;
  ty_env = Neu (Local_var ctx.ty_size) :: ctx.ty_env;
}

(** Extend the context with a term binding *)
let extend_tm (ctx : context) (name : string option) (vty : Core.Semantics.vty) : context = {
  ctx with
  tm_tys = (name, vty) :: ctx.tm_tys;
}

(** Lookup a type name in the context *)
let lookup_ty (ctx : context) (name : string) : Core.index option =
  ctx.ty_names |> List.find_mapi @@ fun index name' ->
    match Some name = name' with
    | true -> Some index
    | false -> None

(** Lookup a term name in the context *)
let lookup_tm (ctx : context) (name : string) : (Core.index * Core.Semantics.vty) option =
  ctx.tm_tys |> List.find_mapi @@ fun index (name', ty) ->
    match Some name = name' with
    | true -> Some (index, ty)
    | false -> None

let eval_ty (ctx : context) (ty : Core.ty) : Core.Semantics.vty =
  Core.Semantics.eval_ty ctx.ty_env ty
let quote_vty (ctx : context) (vty : Core.Semantics.vty) : Core.ty =
  Core.Semantics.quote_vty ctx.ty_size vty
let zonk_ty (ctx: context) (ty : Core.ty) : Core.ty =
  Core.zonk_ty ctx.ty_size ty
let zonk_tm (ctx: context) (tm : Core.tm) : Core.tm =
  Core.zonk_tm ctx.ty_size tm
let pp_ty (ctx : context) (fmt : Format.formatter) (ty : Core.ty) : unit =
  Core.pp_ty ctx.ty_names fmt (zonk_ty ctx ty)

(** {2 Metavariables} *)

(** The reason why a metavariable was inserted *)
type meta_info = [
  | `Forall_arg
  | `Fun_param
  | `Fun_body
  | `If_branches
  | `Placeholder
]

(** A global list of the metavariables inserted during elaboration. This is used
    to generate a list of unsolved metavariables at the end of elaboration. *)
let metas : (Core.meta_id * loc * meta_info) Dynarray.t =
  Dynarray.create ()

(** Generate a fresh metavariable, recording it in the list of metavariables *)
let fresh_meta (ctx : context) (loc: loc) (info : meta_info) : Core.ty =
  let id = Core.Semantics.fresh_meta ctx.ty_size in
  Dynarray.add_last metas (id, loc, info);
  Meta_var id

(** Return a list of unsolved metavariables *)
let unsolved_metas () : (loc * meta_info) list =
  let go (id, loc, info) acc =
    match Core.Semantics.lookup_meta id with
    | Core.Semantics.Unsolved _ -> (loc, info) :: acc
    | Core.Semantics.Solved _ -> acc
  in
  Dynarray.fold_right go metas []


(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of loc * string

(** Raises an {!Error} exception *)
let error (type a) (loc : loc) (message : string) : a =
  raise (Error (loc, message))

let unify_vtys (ctx : context) (loc : loc) (vty1 : Core.Semantics.vty) (vty2 : Core.Semantics.vty) =
  try Core.Semantics.unify_vtys ctx.ty_size vty1 vty2 with
  | Core.Semantics.Mismatched_types (_, _) ->
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          (pp_ty ctx) (quote_vty ctx vty1)
          (pp_ty ctx) (quote_vty ctx vty2))
  | Core.Semantics.Infinite_type id ->
      error loc
        (Format.asprintf "@[<v 2>@[meta variable ?%i refers to itself:@]@ @[expected: %a@]@ @[found: %a@]@]"
          id
          (pp_ty ctx) (quote_vty ctx vty1)
          (pp_ty ctx) (quote_vty ctx vty2))
  | Core.Semantics.Escaping_scope id ->
      error loc
        (Format.asprintf "@[<v 2>@[meta variable ?%i escapes its scope:@]@ @[expected: %a@]@ @[found: %a@]@]"
          id
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
let rec elab_ty (ctx : context) (ty : ty) : Core.ty =
  match ty.data with
  | Name name ->
      begin match lookup_ty ctx name with
      | Some index -> Local_var index
      | None when name = "Bool" -> Bool_type
      | None when name = "Int" -> Int_type
      | None -> error ty.loc (Format.asprintf "unbound type `%s`" name)
      end
  | Placeholder ->
      fresh_meta ctx ty.loc `Placeholder
  | Forall_type (names, body_ty) ->
      let rec go ctx names : Core.ty =
        match names with
        | [] -> elab_ty ctx body_ty
        | name :: names -> Forall_type (name.data, go (extend_ty ctx name.data) names)
      in
      go ctx names
  | Fun_type (ty1, ty2) ->
      Fun_type (elab_ty ctx ty1, elab_ty ctx ty2)

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (ctx : context) (tm : tm) (vty : Core.Semantics.vty) : Core.tm =
  match tm.data with
  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_vty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body = elab_check (extend_tm ctx def_name.data def_vty) body vty in
      Let (def_name.data, quote_vty ctx def_vty, def, body)

  | Fun_lit (params, body) ->
      elab_check_fun_lit ctx params body vty

  | If_then_else (head, tm0, tm1) ->
      let head = elab_check ctx head Bool_type in
      let tm0 = elab_check ctx tm0 vty in
      let tm1 = elab_check ctx tm1 vty in
      Bool_elim (head, tm0, tm1)

  (* Fall back to type inference *)
  | _ ->
      let tm', vty' = elab_infer ctx tm in
      unify_vtys ctx tm.loc vty vty';
      tm'

(** Elaborate a surface term into a core term, inferring its type. *)
and elab_infer (ctx : context) (tm : tm) : Core.tm * Core.Semantics.vty =
  match tm.data with
  | Name name -> begin
      match lookup_tm ctx name with
      | Some (index, vty) -> Local_var index, vty
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
  end

  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_vty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body, body_ty = elab_infer (extend_tm ctx def_name.data def_vty) body in
      Let (def_name.data, quote_vty ctx def_vty, def, body), body_ty

  | Ann (tm, ty) ->
      let ty = elab_ty ctx ty in
      let vty = eval_ty ctx ty in
      elab_check ctx tm vty, vty

  | Bool_lit b ->
      Bool_lit b, Bool_type

  | Int_lit i ->
      Int_lit i, Int_type

  | Fun_lit (params, body) ->
      elab_infer_fun_lit ctx params None body

  | App (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer ctx head in

      let rec go (head : Core.tm) (head_ty : Core.Semantics.vty) arg =
        match Core.Semantics.force head_ty, arg with
        | Forall_type (_, body_ty), Ty_arg arg ->
            let arg = elab_ty ctx arg in
            Core.Forall_app (head, arg), body_ty (eval_ty ctx arg)

        (* Instantiate expected type parameters with metavariables *)
        | Forall_type (_, body_ty), Arg arg ->
            let ty_arg = fresh_meta ctx head_loc `Forall_arg in
            go (Forall_app (head, ty_arg)) (body_ty (eval_ty ctx ty_arg)) (Arg arg)

        | Fun_type (param_ty, body_ty), Arg arg ->
            let arg = elab_check ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty

        | Neu (Meta_var _) as head_ty, Arg arg ->
            let param_ty = eval_ty ctx (fresh_meta ctx head_loc `Fun_param) in
            let body_ty = eval_ty ctx (fresh_meta ctx head_loc `Fun_body) in
            unify_vtys ctx head_loc head_ty (Fun_type (param_ty, body_ty));
            let arg = elab_check ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty

        | head_vty, Ty_arg _ ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: forall@]@ @[found: %a@]@]"
                (pp_ty ctx) (quote_vty ctx head_vty))

        | head_vty, Arg _ ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %a@]@]"
                (pp_ty ctx) (quote_vty ctx head_vty))
      in

      go head head_ty arg

  | If_then_else (head, tm1, tm2) ->
      let head = elab_check ctx head Bool_type in
      let ty = eval_ty ctx (fresh_meta ctx tm.loc `If_branches) in
      let tm1 = elab_check ctx tm1 ty in
      let tm2 = elab_check ctx tm2 ty in
      Bool_elim (head, tm1, tm2), ty

  | Op2 (`Eq, tm0, tm1) ->
      let tm0, vty0 = elab_infer ctx tm0 in
      let tm1, vty1 = elab_infer ctx tm1 in
      unify_vtys ctx tm.loc vty0 vty1;
      begin match Core.Semantics.force vty0 with
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
      let tm0 = elab_check ctx tm0 Int_type in
      let tm1 = elab_check ctx tm1 Int_type in
      Prim_app (prim, [tm0; tm1]), Int_type

  | Op1 (`Neg, tm) ->
      let tm = elab_check ctx tm Int_type in
      Prim_app (Int_neg, [tm]), Int_type

(** Elaborate a function literal into a core term, given an expected type. *)
and elab_check_fun_lit (ctx : context) (params : param list) (body : tm) (vty : Core.Semantics.vty) : Core.tm =
  match params, Core.Semantics.force vty with
  | [], vty ->
      elab_check ctx body vty

  | Ty_param name :: params, Forall_type (_, body_vty) ->
      let ty_var = next_ty_var ctx in
      let body_tm = elab_check_fun_lit (extend_ty ctx name.data) params body (body_vty ty_var) in
      Forall_lit (name.data, body_tm)

  (* Insert missing type parameters *)
  | params, Forall_type (name, body_vty) ->
      let ty_var = next_ty_var ctx in
      let name = name |> Option.map (fun n -> "$" ^ n) in
      let body_tm = elab_check_fun_lit (extend_ty ctx name) params body (body_vty ty_var) in
      Forall_lit (name, body_tm)

  | Param (name, None) :: params, Fun_type (param_vty, body_vty) ->
      let body_tm = elab_check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty in
      Fun_lit (name.data, quote_vty ctx param_vty, body_tm)

  | Param (name, Some param_ty) :: params, Fun_type (param_vty', body_vty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty ctx param_ty in
      let param_vty = eval_ty ctx param_ty in
      unify_vtys ctx param_ty_loc param_vty param_vty';
      Fun_lit (name.data, param_ty,
        elab_check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty)

  | Param (name, _) :: _, Neu (Meta_var _)
  | Ty_param name :: _, Neu (Meta_var _) ->
      let tm', vty' = elab_infer_fun_lit ctx params None body in
      unify_vtys ctx name.loc vty vty';
      tm'

  | Ty_param name :: _, _ ->
      error name.loc "unexpected type parameter"

  | Param (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

(** Elaborate a function literal into a core term, inferring its type. *)
and elab_infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.Semantics.vty =
  let rec go ctx params body_ty body =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = elab_ty ctx body_ty in
        let body_vty = eval_ty ctx body_ty in
        elab_check ctx body body_vty, body_ty

    | [], None ->
        let body, body_vty = elab_infer ctx body in
        body, quote_vty ctx body_vty

    | Ty_param name :: params, body_ty ->
        let body, body_ty = go (extend_ty ctx name.data) params body_ty body in
        Forall_lit (name.data, body), Forall_type (name.data, body_ty)

    | Param (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.loc `Fun_param
          | Some ty -> elab_ty ctx ty
        in
        let body, body_ty =
          let ctx = extend_tm ctx name.data (eval_ty ctx param_ty) in
          elab_infer_fun_lit ctx params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, quote_vty ctx body_ty)
  in

  let body, body_ty = go ctx params body_ty body in
  body, eval_ty ctx body_ty
