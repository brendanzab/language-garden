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

(** Labels that distinguish variant cases, record fields, etc. *)
type label = string located

(** Names that bind definitions or parameters *)
type binder = string option located

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | Fun_type of ty * ty
  | Variant_type of (label * ty) list
  | Placeholder

type pattern =
  pattern_data located

and pattern_data =
  (* | Name of string *)
  (* | Placeholder *)
  (* | Int_lit of int *)
  | Variant_lit of label * binder

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Variant_lit of label * tm
  | Int_lit of int
  | Bool_lit of bool
  | App of tm * tm
  | Match of tm * (pattern * tm) list
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
module Elab = struct

  (** {2 Metavariables} *)

  (** The reason why a metavariable was inserted *)
  type meta_info = [
    | `Fun_param
    | `Fun_body
    | `Variant_lit
    | `Match_clauses
    | `Pattern_binder
    | `If_branches
    | `Placeholder
  ]

  (** A global list of the metavariables inserted during elaboration. This is used
      to generate a list of unsolved metavariables at the end of elaboration. *)
  let metas : (loc * meta_info * Core.meta_state ref) Dynarray.t =
    Dynarray.create ()

  (** Generate a fresh metavariable, recording it in the list of metavariables *)
  let fresh_meta (loc: loc) (info : meta_info) (constr : Core.constr) : Core.ty =
    let state = Core.fresh_meta constr in
    Dynarray.add_last metas (loc, info, state);
    Meta_var state

  (** Return a list of unsolved metavariables *)
  let unsolved_metas () : (loc * meta_info) list =
    let go (loc, info, m) acc =
      match !m with
      | Core.Unsolved (_, Any) -> (loc, info) :: acc
      | Core.Unsolved (_, Variant row) ->
          (* Default to a concrete variant type *)
          m := Solved (Variant_type row);
          acc
      | Core.Solved _ -> acc
    in
    Dynarray.fold_right go metas []


  (** {2 Elaboration context} *)

  (** A stack of bindings currently in scope *)
  type context = (string option * Core.ty) Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx |> List.find_mapi @@ fun index (name', ty) ->
      match Some name = name' with
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

  let unify (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    try Core.unify ty1 ty2 with
    | Core.Infinite_type _ -> error loc "infinite type"
    | Core.Mismatched_types (_, _) ->
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
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        error ty.loc (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ty1, check_ty ty2)
    | Variant_type row ->
        Variant_type
          (List.fold_left
            (fun acc (label, ty) ->
              if Core.Label_map.mem label.data acc then
                error label.loc (Format.asprintf "duplicate label `%s`" label.data)
              else
                Core.Label_map.add label.data (check_ty ty) acc)
            Core.Label_map.empty
            row)
    | Placeholder ->
        fresh_meta ty.loc `Placeholder Any

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data, Core.force ty with
    | Let (def_name, params, def_body_ty, def_body, body), body_ty ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm ((def_name.data, def_ty) :: ctx) body body_ty in
        Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body), ty ->
        check_fun_lit ctx params body ty

    | Variant_lit (label, tm), Variant_type row -> begin
        match Core.Label_map.find_opt label.data row with
        | Some elem_ty ->
            Variant_lit (label.data, check_tm ctx tm elem_ty, ty)
        | None ->
            error label.loc
              (Format.asprintf "unexpected variant `%s` in type `%a`"
                label.data
                Core.pp_ty ty)
    end

    | Match (head, clauses), body_ty ->
        check_tm_match ctx head clauses body_ty

    | If_then_else (head, tm1, tm2), ty ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        (* NOTE: We could add some subtyping coercions here *)
        unify tm.loc ty ty';
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, vty) -> Var index, vty
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm ((def_name.data, def_ty) :: ctx) body in
        Let (def_name.data, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | Variant_lit (label, elem_tm) ->
        let elem_tm, elem_ty = infer_tm ctx elem_tm in
        let row = Core.Label_map.singleton label.data elem_ty in
        let ty = fresh_meta tm.loc `Variant_lit (Variant row) in
        Variant_lit (label.data, elem_tm, ty), ty

    | Int_lit i ->
        Int_lit i, Int_type

    | Bool_lit b ->
        Bool_lit b, Bool_type

    | App (head, arg) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in
        let param_ty, body_ty =
          match Core.force head_ty with
          | Fun_type (param_ty, body_ty) -> param_ty, body_ty
          | head_ty ->
              let param_ty = fresh_meta head_loc `Fun_param Any in
              let body_ty = fresh_meta head_loc `Fun_body Any in
              unify head_loc head_ty (Fun_type (param_ty, body_ty));
              param_ty, body_ty
        in
        let arg = check_tm ctx arg param_ty in
        Fun_app (head, arg), body_ty

    | Match (head, clauses) ->
        let body_ty = fresh_meta tm.loc `Match_clauses Any in
        check_tm_match ctx head clauses body_ty, body_ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let ty = fresh_meta tm.loc `If_branches Any in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2), ty

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, ty0 = infer_tm ctx tm0 in
        let tm1, ty1 = infer_tm ctx tm1 in
        unify tm.loc ty0 ty1;
        begin match Core.force ty0 with
        | Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | ty -> error tm.loc (Format.asprintf "@[unsupported type: %a@]" Core.pp_ty ty)
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
    match params, Core.force ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_loc = param_ty.loc in
        let param_ty = check_ty param_ty in
        unify param_ty_loc param_ty param_ty';
        let body = check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify name.loc ty ty';
        tm'
    | (name, _) :: _, _ ->
        error name.loc "unexpected parameter"

  (** Elaborate a function literal, inferring its type. *)
  and infer_fun_lit (context : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty body_ty in
        check_tm context body body_ty, body_ty
    | [], None ->
        infer_tm context body
    | (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta name.loc `Fun_param Any
          | Some ty -> check_ty ty
        in
        let body, body_ty = infer_fun_lit ((name.data, param_ty) :: context) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)

  (** Elaborate a pattern match, checking the clause bodies against an expected type. *)
  and check_tm_match (ctx : context) (head : tm) (clauses : (pattern * tm) list) (body_ty : Core.ty) : Core.tm =
    let head_loc = head.loc in
    let head, head_ty = infer_tm ctx head in
    (* TDOD: Proper match compilation *)
    match Core.force head_ty with
    | Variant_type row ->
        (* iterate through clauses, accumulating cases *)
        let cases =
          List.fold_left
            (fun cases ({ data = Variant_lit (label, name); _}, body_tm : pattern * _) ->
              if Core.Label_map.mem label.data cases then
                (* TODO: should be a warning *)
                error label.loc (Format.asprintf "redundant variant pattern `%s`" label.data)
              else
                match Core.Label_map.find_opt label.data row with
                | Some case_ty ->
                    let body_tm = check_tm ((name.data, case_ty) :: ctx) body_tm body_ty in
                    Core.Label_map.add label.data (name.data, body_tm) cases
                | None ->
                    error label.loc (Format.asprintf "unexpected variant pattern `%s`" label.data))
            Core.Label_map.empty
            clauses
        in
        (* check that labels in the cases match the labels in the row *)
        let missing_cases =
          Core.Label_map.to_seq row
          |> Seq.filter (fun (label, _) -> not (Core.Label_map.mem label cases))
          |> List.of_seq
        in
        if List.is_empty missing_cases then
          (* return cases *)
          Variant_elim (head, cases)
        else
          error head_loc
            (Format.asprintf "non-exhaustive match, missing %a"
              (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                (fun fmt (label, _) -> Format.fprintf fmt "`%s`" label))
              missing_cases)

    | head_ty ->
        (* Build up the cases and the row from the clauses *)
        let cases, row =
          List.fold_left
            (fun (cases, row) ({ data = Variant_lit (label, name); _}, body_tm : pattern * _) ->
              if Core.Label_map.mem label.data cases then
                (* TODO: should be a warning? *)
                error label.loc (Format.asprintf "redundant variant pattern `%s`" label.data)
              else
                let case_ty = fresh_meta name.loc `Pattern_binder Any in
                let body_tm = check_tm ((name.data, case_ty) :: ctx) body_tm body_ty in
                Core.Label_map.add label.data (name.data, body_tm) cases,
                Core.Label_map.add label.data case_ty row)
            (Core.Label_map.empty, Core.Label_map.empty)
            clauses
        in
        (* Unify head type with variant type *)
        unify head_loc head_ty (Variant_type row);
        (* return cases *)
        Variant_elim (head, cases)

end
