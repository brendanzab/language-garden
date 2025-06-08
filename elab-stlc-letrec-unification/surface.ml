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
  | Let of defn * tm
  | Let_rec of defn list * tm
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

(** Definitions *)
and defn =
  binder * param list * ty option * tm


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab = struct

  (** {2 Metavariables} *)

  (** The reason why a metavariable was inserted *)
  type meta_info = [
    | `Fun_param
    | `Fun_body
    | `If_branches
    | `Placeholder
  ]

  (** A global list of the metavariables inserted during elaboration. This is used
      to generate a list of unsolved metavariables at the end of elaboration. *)
  let metas : (loc * meta_info * Core.meta_state ref) Dynarray.t =
    Dynarray.create ()

  (** Generate a fresh metavariable, recording it in the list of metavariables *)
  let fresh_meta (loc: loc) (info : meta_info) : Core.ty =
    let state = Core.fresh_meta () in
    Dynarray.add_last metas (loc, info, state);
    Meta_var state

  (** Return a list of unsolved metavariables *)
  let unsolved_metas () : (loc * meta_info) list =
    let go (loc, info, m) acc =
      match !m with
      | Core.Unsolved _ -> (loc, info) :: acc
      | Core.Solved _ -> acc
    in
    Dynarray.fold_right go metas []


  (** {2 Elaboration context} *)

  (** An entry in the context *)
  type entry =
    | Def of string option * Core.ty
    | Mutual_def of (string option * Core.ty) list

  (** A stack of bindings currently in scope *)
  type context = entry Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.tm * Core.ty) option =
    ctx |> List.find_mapi @@ fun index entry ->
      match entry with
      | Def (name', ty) when Some name = name' ->
          Some (Core.Var index, ty)
      | Def (_, _) -> None
      | Mutual_def entries ->
          entries |> List.find_mapi @@ fun elem_index (name', ty) ->
            match Some name = name' with
            | true -> Some (Core.Tuple_proj (Var index, elem_index), ty)
            | false -> None


  (** {2 Elaboration errors} *)

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of loc * string

  (** Raises an {!Error} exception *)
  let error (type a) (loc : loc) (message : string) : a =
    raise (Error (loc, message))

  let unify_tys (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    try Core.unify_tys ty1 ty2 with
    | Core.Infinite_type _ -> error loc "infinite type"
    | Core.Mismatched_types (_, _) ->
        error loc
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[found: %t@]@]"
            (Core.pp_ty ty1)
            (Core.pp_ty ty2))


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        error ty.loc (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ty1, check_ty ty2)
    | Placeholder ->
        fresh_meta ty.loc `Placeholder

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body = check_tm (Def (def_name.data, def_ty) :: ctx) body ty in
        Let (def_name.data, def_ty, def, body)

    | Let_rec (defns, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defns ctx defns in
        let body = check_tm (defs_entry :: ctx) body ty in
        Let (def_name, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body ty

    | If_then_else (head, tm0, tm1) ->
        let head = check_tm ctx head Bool_type in
        let tm0 = check_tm ctx tm0 ty in
        let tm1 = check_tm ctx tm1 ty in
        Bool_elim (head, tm0, tm1)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        unify_tys tm.loc ty ty';
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (tm, ty) -> tm, ty
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
        end

    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body, body_ty = infer_tm (Def (def_name.data, def_ty) :: ctx) body in
        Let (def_name.data, def_ty, def, body), body_ty

    | Let_rec (defns, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defns ctx defns in
        let body, body_ty = infer_tm (defs_entry :: ctx) body in
        Let (def_name, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ty in
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
        let param_ty, body_ty =
          match Core.force_ty head_ty with
          | Fun_type (param_ty, body_ty) -> param_ty, body_ty
          | head_ty ->
              let param_ty = fresh_meta head_loc `Fun_param in
              let body_ty = fresh_meta head_loc `Fun_body in
              unify_tys head_loc (Fun_type (param_ty, body_ty)) head_ty;
              param_ty, body_ty
        in
        let arg = check_tm ctx arg param_ty in
        Fun_app (head, arg), body_ty

    | If_then_else (head, tm0, tm1) ->
        let head = check_tm ctx head Bool_type in
        let ty = fresh_meta tm.loc `If_branches in
        let tm0 = check_tm ctx tm0 ty in
        let tm1 = check_tm ctx tm1 ty in
        Bool_elim (head, tm0, tm1), ty

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, ty0 = infer_tm ctx tm0 in
        let tm1, ty1 = infer_tm ctx tm1 in
        unify_tys tm.loc ty0 ty1;
        begin match Core.force_ty ty0 with
        | Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | ty -> error tm.loc (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty))
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
    match params, Core.force_ty ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit (Def (name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_loc = param_ty.loc in
        let param_ty = check_ty param_ty in
        unify_tys param_ty_loc param_ty param_ty';
        let body = check_fun_lit (Def (name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify_tys name.loc ty ty';
        tm'
    | (name, _) :: _, _ ->
        error name.loc "unexpected parameter"

  (** Elaborate a function literal, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, None) :: params, body_ty ->
        let param_ty = fresh_meta name.loc `Fun_param in
        let body, body_ty = infer_fun_lit (Def (name.data, param_ty) :: ctx) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty param_ty in
        let body, body_ty = infer_fun_lit (Def (name.data, param_ty) :: ctx) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)

  (** Elaborate the definitions of a recursive let binding. *)
  and elab_let_rec_defns (ctx : context) (defns : defn list) : string option * entry * Core.ty * Core.tm =
    (* Creates a fresh function type for a definition. *)
    let rec fresh_fun_ty ctx loc params body_ty =
      match params, body_ty with
      | [], Some body_ty -> check_ty body_ty
      | [], None -> fresh_meta loc `Fun_body
      | (name, _) :: params, body_ty ->
          let param_ty = fresh_meta name.loc `Fun_param in
          Fun_type (param_ty, fresh_fun_ty (Def (name.data, param_ty) :: ctx) loc params body_ty)
    in

    (* Check the body of a definition, ensuring it is a function *)
    let check_def_body ctx params def_loc def def_ty =
      (* Avoid “vicious circles” with a blunt syntactic check on the elaborated
        term, ensuring that it is a function literal. This is similar to the
        approach used in Standard ML. This rules out definitions like:

            let x := x + 1;

        OCaml uses a more complicated check (as of versions 4.06 and 4.08) that
        admits more valid programs. A detailed description of this check can be
        found in “A practical mode system for recursive definitions” by Reynaud
        et. al. https://doi.org/10.1145/3434326. *)
      match check_fun_lit ctx params def def_ty with
      | Fun_lit _ as def_body -> def_body
      | _ -> error def_loc "expected function literal in recursive let binding"
    in

    (* Check a series of mutually recursive definitions against their types *)
    let check_def_bodies ctx =
      List.map2 (fun (def_name, params, _, def) (_, def_ty) ->
        check_def_body ctx params def_name.loc def def_ty)
    in

    (* Elaborate the recursive definitions, special-casing singly recursive
      definitions. The special-casing is not exactly necessary, but does reduce
      indirections during evaluation. *)
    match defns with
    (* Singly recursive definitions *)
    | [(def_name, params, def_ty, def)] ->
        let def_ty = fresh_fun_ty ctx def_name.loc params def_ty in
        let def_entry = Def (def_name.data, def_ty) in
        let def_body = check_def_body (def_entry :: ctx) params def_name.loc def def_ty in
        let def = Core.Fix (def_name.data, def_ty, def_body) in

        def_name.data, def_entry, def_ty, def

    (* Mutually recursive definitions *)
    | defs ->
        let def_tys = defs |> List.map (fun (def_name, params, def_ty, _) ->
          def_name.data, fresh_fun_ty ctx def_name.loc params def_ty)
        in
        let defs_entry = Mutual_def def_tys in
        let defs = check_def_bodies (defs_entry :: ctx) defs def_tys in

        (* Create the combined mutual definition using a tuple and the fixed-point
          combinator. Alternatively we could use a record here, which could make
          debugging the elaborated terms a little easier. *)
        let def_name = Printf.sprintf "$mutual-%i" (List.length ctx) in
        let def_ty = Core.Tuple_type (List.map snd def_tys) in
        let def = Core.Fix (Some def_name, def_ty, Tuple_lit defs) in

        Some def_name, defs_entry, def_ty, def

end
