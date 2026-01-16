(** {0 Core language} *)

module Ty = struct

  (** Names of bound type variables *)
  type name = string

  (** Identifier used for pretty printing metavariables. *)
  type meta_id = int

  (** Type syntax *)
  type t =
    | Var of name         (* Type variables (bound by type parameters) *)
    | Meta_var of meta
    | Fun of t * t
    | Tuple of t list
    | Int
    | Bool

  (** The current state of a metavariable *)
  and meta_state =
    | Solved of t
    | Unsolved of meta_id

  (** Mutable representation of metavariables. These are updated in-place during
      unification and when types are forced. Alternatively we could have
      chosen to store these in a separate metacontext, like in the
      elaboration-zoo. *)
  and meta = meta_state ref

  (** Replace bound variables in a type *)
  let rec subst (mapping : (string * t) list) (ty : t) : t =
    match ty with
    | Var name -> List.assoc_opt name mapping |> Option.value ~default:ty
    | Meta_var { contents = Solved ty } -> subst mapping ty
    | Meta_var { contents = Unsolved _ } -> ty
    | Fun (param_ty, body_ty) -> Fun (subst mapping param_ty, subst mapping body_ty)
    | Tuple elem_tys -> Tuple (List.map (subst mapping) elem_tys)
    | Bool -> Bool
    | Int -> Int


  (** {1 Functions related to metavariables} *)

  (** Create a fresh, unsolved metavariable *)
  let fresh_meta : unit -> meta =
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      ref (Unsolved id)

  (** Force any solved metavariables on the outermost part of a type. Chains of
      metavariables will be collapsed to make forcing faster in the future. This
      is sometimes referred to as {i path compression}. *)
  let rec force (ty : t) : t =
    match ty with
    | Meta_var ({ contents = Solved ty } as m) ->
        let ty = force ty in
        m := Solved ty;
        ty
    | ty -> ty


  (** {1 Unification} *)

  exception Infinite_type of meta
  exception Mismatched_types of t * t

  (** Occurs check. This guards against self-referential unification problems
      that would result in infinite loops during unification. *)
  let rec occurs (m : meta) (ty : t) : unit =
    match force ty with
    | Var _ -> ()
    | Meta_var m' ->
        if m == m' then
          raise (Infinite_type m)
    | Fun (param_ty, body_ty) ->
        occurs m param_ty;
        occurs m body_ty
    | Tuple elem_tys ->
        List.iter (occurs m) elem_tys
    | Int -> ()
    | Bool -> ()

  (** Check if two types are the same, updating unsolved metavariables in one
      type with known information from the other type if possible. *)
  let rec unify (ty1 : t) (ty2 : t) : unit =
    match force ty1, force ty2 with
    | Var name1, Var name2 when name1 = name2 -> ()
    | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
    | Meta_var m, ty | ty, Meta_var m ->
        occurs m ty;
        m := Solved ty
    | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
        unify param_ty1 param_ty2;
        unify body_ty1 body_ty2
    | Tuple [], Tuple [] -> ()
    | Tuple (elem_ty1 :: elem_tys1), Tuple (elem_ty2 :: elem_tys2) ->
        unify elem_ty1 elem_ty2;
        unify (Tuple elem_tys1) (Tuple elem_tys2)
    | Int, Int -> ()
    | Bool, Bool -> ()
    | ty1, ty2 ->
        raise (Mismatched_types (ty1, ty2))


  (** {1 Pretty printing} *)

  let pp : t -> Format.formatter -> unit =
    let rec pp_ty ty ppf =
      match ty with
      | Meta_var m -> pp_meta pp_ty m ppf
      | Fun (param_ty, body_ty) ->
          Format.fprintf ppf "%t -> %t"
            (pp_atomic_ty param_ty)
            (pp_ty body_ty)
      | ty ->
          pp_atomic_ty ty ppf
    and pp_atomic_ty ty ppf =
      match ty with
      | Var name -> Format.fprintf ppf "%s" name
      | Meta_var m -> pp_meta pp_atomic_ty m ppf
      | Tuple [] -> Format.fprintf ppf "()"
      | Tuple [elem_ty] -> Format.fprintf ppf "@[(%t,)@]" (pp_ty elem_ty)
      | Tuple elem_tys ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[(%t)@]"
            (Fun.flip (Format.pp_print_list (Fun.flip pp_ty) ~pp_sep) elem_tys)
      | Int -> Format.fprintf ppf "Int"
      | Bool -> Format.fprintf ppf "Bool"
      | Fun _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
    and pp_meta pp_ty m ppf =
      match !m with
      | Solved ty -> pp_ty ty ppf
      | Unsolved id -> Format.fprintf ppf "?%i" id
    in
    pp_ty

end


module Tm = struct

  (** These names are used as hints for pretty printing binders and variables,
      but don’t impact the equality of terms. *)
  type name = string option

  (** {i De Bruijn index} that represents a variable occurrence by the number of
      binders between the occurrence and the binder it refers to. *)
  type index = int

  (** An environment of bindings that can be looked up directly using a
      {!index}, or by converting to a {!level} using {!level_to_index}. *)
  type 'a env = 'a list

  (** Term syntax *)
  type t =
    | Var of index * Ty.t list
    | Let of def * t
    | Let_rec of def list * t
    | Fun_lit of name * Ty.t * t
    | Fun_app of t * t
    | Tuple_lit of t list
    | Tuple_proj of t * int
    | Int_lit of int
    | Bool_lit of bool
    | Bool_elim of t * t * t
    | Prim_app of Prim.t * t list

  and def =
    name * Ty.name list * Ty.t * t

  let pp : name env -> t -> Format.formatter -> unit =
    let pp_name (name : name) (ppf : Format.formatter) : unit =
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None -> Format.pp_print_string ppf "_"
    in

    let pp_name_ann (name : name) (ty_params : Ty.name list) (ty : Ty.t) (ppf : Format.formatter) : unit =
      match ty_params with
      | [] -> Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (Ty.pp ty)
      | ty_params ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[<2>@[%t@ @[[%t]@]@ :@]@ %t@]"
            (pp_name name)
            (Fun.flip (Format.pp_print_list Format.pp_print_string ~pp_sep) ty_params)
            (Ty.pp ty)

    and pp_param (name : name) (ty : Ty.t) (ppf : Format.formatter) : unit =
      Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (Ty.pp ty)
    in

    let rec pp_tm names tm ppf =
      match tm with
      | Let _ | Let_rec _ as tm ->
          let rec go names tm ppf =
            match tm with
            | Let ((name, ty_params, def_ty, def), body) ->
                Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                  (pp_name_ann name ty_params def_ty)
                  (pp_tm names def)
                  (go (name :: names) body)
            | Let_rec (defs, body) ->
                let extend names (n, _, _, _) = n :: names in
                let rec_names = List.fold_left extend names defs in
                (* FIXME: indentation *)
                Format.fprintf ppf "@[@[let@ rec@ {@]%t@ };@]@ %t"
                  (defs |> Fun.flip @@ Format.pp_print_list
                    ~pp_sep:Format.pp_print_nothing
                    (fun ppf (name, ty_params, def_ty, def) ->
                      Format.fprintf ppf "@;<1 2>@[<2>@[%t@ :=@]@ %t@];"
                        (pp_name_ann name ty_params def_ty)
                        (pp_tm rec_names def)))
                  (go rec_names body)
            | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
          in
          Format.fprintf ppf "@[<v>%t@]" (go names tm)
      | Fun_lit (name, param_ty, body) ->
          let rec go names tm ppf =
            match tm with
            | Fun_lit (name, param_ty, body) ->
                Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                  (pp_param name param_ty)
                  (go (name :: names) body)
            | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm names tm)
          in
          Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
            (pp_param name param_ty)
            (go (name :: names) body)
      | Bool_elim (head, tm1, tm2) ->
          Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
            (pp_app_tm names head)
            (pp_app_tm names tm1)
            (pp_tm names tm2)
      | tm ->
          pp_app_tm names tm ppf
    and pp_app_tm names tm ppf =
      let rec go tm ppf =
        match tm with
        | Fun_app (head, arg) ->
            Format.fprintf ppf "%t@ %t"
              (go head)
              (pp_proj_tm names arg)
        | Prim_app (prim, args) ->
            let pp_sep ppf () = Format.fprintf ppf "@ " in
            Format.fprintf ppf "#%s@ %t"
              (Prim.name prim)
              (Fun.flip (Format.pp_print_list (Fun.flip (pp_proj_tm names)) ~pp_sep) args)
        | tm ->
            pp_proj_tm names tm ppf
      in
      match tm with
      | Fun_app _ | Prim_app _ as tm ->
          Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
      | tm ->
          pp_proj_tm names tm ppf
    and pp_proj_tm names tm ppf =
      let rec go tm ppf =
        match tm with
        | Tuple_proj (head, label) ->
              Format.fprintf ppf "%t@,.%i" (go head) label
        | tm ->
            pp_atomic_tm names tm ppf
      in
      match tm with
      | Tuple_proj _ as tm ->
          Format.fprintf ppf "@[<2>%t@]" (go tm)
      | tm ->
          pp_atomic_tm names tm ppf
    and pp_atomic_tm names tm ppf =
      match tm with
      | Var (index, []) -> Format.fprintf ppf "%t" (pp_name (List.nth names index))
      | Var (index, ((_ :: _) as ty_args)) ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "%t@ @[[%t]@]"
            (pp_name (List.nth names index))
            (Fun.flip (Format.pp_print_list (Fun.flip Ty.pp) ~pp_sep) ty_args)
      | Tuple_lit [] -> Format.fprintf ppf "()"
      | Tuple_lit [elem] -> Format.fprintf ppf "@[(%t,)@]" (pp_tm names elem)
      | Tuple_lit elems ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[(%t)@]"
            (Fun.flip (Format.pp_print_list (Fun.flip (pp_tm names)) ~pp_sep) elems)
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Let _ | Let_rec _ | Fun_lit _ | Fun_app _ | Tuple_proj _ | Bool_elim _
      | Prim_app _ as tm ->
          Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
    in
    pp_tm

  module Value = struct

    type t =
      | Fun_lit of (t -> t)
      | Tuple_lit of t list
      | Int_lit of int
      | Bool_lit of bool

    let rec pp (vexpr : t) (ppf : Format.formatter) : unit =
      match vexpr with
      | Fun_lit _ -> Format.fprintf ppf "<function>"
      | Tuple_lit [] -> Format.fprintf ppf "()"
      | Tuple_lit [elem] -> Format.fprintf ppf "@[(%t)@]" (pp elem)
      | Tuple_lit elems ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[(%t)@]"
            (Fun.flip (Format.pp_print_list (Fun.flip pp) ~pp_sep) elems)
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Int_lit i -> Format.fprintf ppf "%i" i

  end

  let rec eval (env : Value.t ref env) (tm : t) : Value.t =
    match tm with
    | Var (index, _) -> !(List.nth env index)
    | Let ((_, _, _, def), body) ->
        let def = eval env def in
        eval (ref def :: env) body
    | Let_rec (defs, body) ->
        (* Add placeholder bindings to the environment then back-patch them
           with their corresponding recursive definitions. This is inspired by
           Landin’s approach in “The Mechanical Evaluation of Expressions” and
           R⁵RS Scheme’s approach to encoding [letrec] (see Section 7.3 of the
           {{: https://dl.acm.org/doi/10.1145/290229.290234} Revised⁵ Report
           on the Algorithmic Language Scheme}).

           This relies on each definition being a function in order to avoid
           the bindings from being accessed before they have been defined. *)
        let undefined = Value.Fun_lit (fun _ -> failwith "undefined") in
        let bindings = defs |> List.map (fun _ -> ref undefined) in
        let env = List.rev_append bindings env in
        List.iter2 (fun vdef (_, _, _, tm) -> vdef := eval env tm) bindings defs;
        eval env body
    | Fun_lit (_, _, body) ->
        Value.Fun_lit (fun arg -> eval (ref arg :: env) body)
    | Fun_app (head, arg) ->
        begin match eval env head with
        | Value.Fun_lit vbody -> vbody (eval env arg)
        | _ -> invalid_arg "eval"
        end
    | Tuple_lit elems ->
        Value.Tuple_lit (List.map (eval env) elems)
    | Tuple_proj (head, index) ->
        begin match eval env head with
        | Value.Tuple_lit velems -> List.nth velems index
        | _ -> invalid_arg "eval"
        end
    | Int_lit i -> Value.Int_lit i
    | Bool_lit b -> Value.Bool_lit b
    | Bool_elim (head, true_body, false_body) ->
        begin match eval env head with
        | Value.Bool_lit true -> eval env true_body
        | Value.Bool_lit false -> eval env false_body
        | _ -> invalid_arg "eval"
        end
    | Prim_app (prim, args) ->
        begin match prim, List.map (eval env) args with
        | Prim.Bool_eq, Value.[Bool_lit t1; Bool_lit t2] -> Value.Bool_lit (Bool.equal t1 t2)
        | Prim.Int_eq, Value.[Int_lit t1; Int_lit t2] -> Value.Bool_lit (Int.equal t1 t2)
        | Prim.Int_add, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.add t1 t2)
        | Prim.Int_sub, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.sub t1 t2)
        | Prim.Int_mul, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.mul t1 t2)
        | Prim.Int_neg, Value.[Int_lit t1] -> Value.Int_lit (Int.neg t1)
        | _ -> invalid_arg "eval"
        end

end
