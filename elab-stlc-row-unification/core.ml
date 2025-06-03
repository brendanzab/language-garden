(** {0 Core language} *)

(** {1 Names} *)

(** Labels are significant to the equality of terms. They are typically used
    to distinguish elements in variants, records, etc. *)
type label = string

(** An unordered row of elements distinguished by label. *)
module Label_map = Map.Make (String)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurrence by the number of
    binders between the occurrence and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurrence by the number of
    binders from the top of the environment to the binder that the occurrence
    refers to. These do not change their meaning as new bindings are added to
    the environment. *)
type level = int

(** [level_to_index size level] converts [level] to an {!index} that is bound in
    an environment of the supplied [size], where [size] represents the next
    fresh {!level} to be bound in the environment.

    Assumes that [size > level].
*)
let level_to_index (size : level) (level : level) =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Identifier used for pretty printing metavariables. *)
type meta_id = int

(** Identifier used for pretty printing row metavariables. *)
type row_meta_id = int

(** Type syntax *)
type ty =
  | Meta_var of meta
  | Fun_type of ty * ty
  | Record_type of row_ty
  | Variant_type of row_ty
  | Int_type
  | Bool_type

(** The current state of a metavariable *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id

(** Mutable representation of metavariables. These are updated in-place during
    unification and when types are forced. Alternatively we could have
    chosen to store these in a separate metacontext, like in the
    elaboration-zoo. *)
and meta = meta_state ref

(** Row type syntax *)
and row_ty =
  | Row_meta_var of row_meta
  | Row_entries of ty Label_map.t

(** The current state of a row metavariable *)
and row_meta_state =
  | Solved_row of row_ty
  | Unsolved_row of row_meta_id * ty Label_map.t

(* Mutable representation of row metavariables. *)
and row_meta = row_meta_state ref

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Record_lit of tm Label_map.t
  | Record_proj of tm * label
  | Variant_lit of label * tm * ty
  | Variant_elim of tm * (name * tm) Label_map.t
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (vtm -> vtm)
    | Record_lit of vtm Label_map.t
    | Variant_lit of label * vtm * ty
    | Bool_lit of bool
    | Int_lit of int

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Record_proj of ntm * label
    | Variant_elim of ntm * (name * (vtm -> vtm)) Label_map.t
    | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let record_proj head label =
    match head with
    | Neu ntm -> Neu (Record_proj (ntm, label))
    | Record_lit fields -> Label_map.find label fields
    | _ -> invalid_arg "expected record"

  let variant_elim head clauses =
    match head with
    | Neu ntm -> Neu (Variant_elim (ntm, clauses))
    | Variant_lit (label, vtm, _) ->
        let _, body = Label_map.find label clauses in
        body vtm
    | _ -> invalid_arg "expected variant"

  let bool_elim head vtm1 vtm2 =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> vtm1 ()
    | Bool_lit false -> vtm2 ()
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, fun arg -> eval (arg :: env) body)
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Record_lit fields ->
        Record_lit (fields |> Label_map.map (eval env))
    | Record_proj (head, label) ->
        let head = eval env head in
        record_proj head label
    | Variant_lit (label, tm, ty) ->
        Variant_lit (label, eval env tm, ty)
    | Variant_elim (head, clauses) ->
        let head = eval env head in
        let clauses =
          clauses |> Label_map.map (fun (name, body) ->
            name, fun arg -> eval (arg :: env) body)
        in
        variant_elim head clauses
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm0, tm1) ->
        let head = eval env head in
        let vtm1 () = eval env tm0 in
        let vtm2 () = eval env tm1 in
        bool_elim head vtm1 vtm2
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Record_lit fields ->
        Record_lit (fields |> Label_map.map (quote size))
    | Variant_lit (label, vtm, ty) ->
        Variant_lit (label, quote size vtm, ty)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Record_proj (head, label) ->
        Record_proj (quote_neu size head, label)
    | Variant_elim (head, clauses) ->
        let head = quote_neu size head in
        let clauses =
          clauses |> Label_map.map (fun (name, body) ->
            name, quote (size + 1) (body (Neu (Var size))))
        in
        Variant_elim (head, clauses)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm0 = quote size (vtm1 ()) in
        let tm1 = quote size (vtm2 ()) in
        Bool_elim (quote_neu size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable *)
let fresh_meta : unit -> meta =
  let next_id = ref 0 in
  fun () ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved id)

(** Create a fresh, unsolved row metavariable with an initial row constraint *)
let fresh_row_meta : ty Label_map.t -> row_meta =
  let next_id = ref 0 in
  fun row ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved_row (id, row))

(** Force any solved metavariables and row metavariables on the outermost part
    of a type. Chains of metavariables will be collapsed to make forcing faster
    in the future. This is sometimes referred to as {i path compression}. *)
let rec force_ty (ty : ty) : ty =
  match ty with
  | Meta_var ({ contents = Solved ty } as m) ->
      let ty = force_ty ty in
      m := Solved ty;
      ty
  | Record_type rty -> Record_type (force_row_ty rty)
  | Variant_type rty -> Variant_type (force_row_ty rty)
  | ty -> ty

(** Force row metavariables on the on the outermost part of a row type  *)
and force_row_ty (rty : row_ty) : row_ty =
  match rty with
  | Row_meta_var ({ contents = Solved_row rty } as rm) ->
      let rty = force_row_ty rty in
      rm := Solved_row rty;
      rty
  | ty -> ty


(** {1 Unification} *)

exception Infinite_type of meta
exception Mismatched_types of ty * ty
exception Infinite_row_type of row_meta
exception Mismatched_row_types of row_ty * row_ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs_ty (m : meta) (ty : ty) : unit =
  match force_ty ty with
  | Meta_var m' ->
      if m == m' then
        raise (Infinite_type m)
  | Fun_type (param_ty, body_ty) ->
      occurs_ty m param_ty;
      occurs_ty m body_ty
  | Record_type rty | Variant_type rty ->
      begin match force_row_ty rty with
      | Row_entries row -> row |> Label_map.iter (fun _ -> occurs_ty m)
      | Row_meta_var _ -> ()
      end
  | Int_type -> ()
  | Bool_type -> ()

(** The same as [occurs_ty], but guards against self-referential row types *)
let occurs_row_ty (rm : row_meta) (rty : row_ty) : unit =
  let rec go_ty ty =
    match force_ty ty with
    | Meta_var _ -> ()
    | Fun_type (param_ty, body_ty) ->
        go_ty param_ty;
        go_ty body_ty
    | Record_type rty -> go_row_ty rty
    | Variant_type rty -> go_row_ty rty
    | Int_type -> ()
    | Bool_type -> ()
  and go_row_ty rty =
    match force_row_ty rty with
    | Row_meta_var rm' ->
        if rm == rm' then
          raise (Infinite_row_type rm)
    | Row_entries row ->
        row |> Label_map.iter (fun _ -> go_ty)
  in
  go_row_ty rty

(** Check if two types are the same, updating unsolved metavariables in one
    type with known information from the other type if possible. *)
let rec unify_tys (ty1 : ty) (ty2 : ty) : unit =
  match force_ty ty1, force_ty ty2 with
  | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
  | Meta_var m, ty | ty, Meta_var m ->
      occurs_ty m ty;
      m := Solved ty
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      unify_tys param_ty1 param_ty2;
      unify_tys body_ty1 body_ty2
  | Record_type rty1, Record_type rty2
  | Variant_type rty1, Variant_type rty2 ->
      unify_row_tys rty1 rty2
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | ty1, ty2 ->
      raise (Mismatched_types (ty1, ty2))

(** Unify row types, updating row constraints as needed *)
and unify_row_tys (rty1 : row_ty) (rty2 : row_ty) : unit =
  (* Unifies the types in two row constraints, merging them into a single constraint *)
  let merge_rows =
    Label_map.merge @@ fun _ ty1 ty2 ->
      match ty1, ty2 with
      | Some ty1, Some ty2 -> unify_tys ty1 ty2; Some ty1
      | Some ty, None | None, Some ty -> Some ty
      | None, None  -> None
  in

  match force_row_ty rty1, force_row_ty rty2 with
  | Row_meta_var rm1, Row_meta_var rm2 when rm1 == rm2 -> ()

  (* Should never occur *)
  | Row_meta_var { contents = Solved_row _ }, _
  | _, Row_meta_var { contents = Solved_row _ } ->
      failwith "expected forced row type"

  (* Unify a row constraint against a concrete row type *)
  | Row_meta_var ({ contents = Unsolved_row (_, row) } as rm), (Row_entries exact_row as rty)
  | (Row_entries exact_row as rty), Row_meta_var ({ contents = Unsolved_row (_, row) } as rm) ->
      occurs_row_ty rm rty;
      (* Unify the types in the unsolved row against the types in the concrete
        row, failing if any are missing. *)
      row |> Label_map.iter begin fun label row_ty ->
        match Label_map.find_opt label exact_row with
        | Some exact_row_ty -> unify_tys row_ty exact_row_ty
        | None -> raise (Mismatched_row_types (Row_meta_var rm, rty))
      end;
      rm := Solved_row rty

  (* Unify two unsolved rows *)
  | Row_meta_var ({ contents = Unsolved_row (id, row1) } as rm1),
    (Row_meta_var ({ contents = Unsolved_row (_, row2) } as rm2) as rty)->
      occurs_row_ty rm1 rty;
      rm2 := Unsolved_row (id, merge_rows row1 row2);
      rm1 := Solved_row rty

  (* Unify concrete row types *)
  | Row_entries row1, Row_entries row2 ->
      if not (Label_map.equal (fun ty1 ty2 -> unify_tys ty1 ty2; true) row1 row2) then
        raise (Mismatched_row_types (rty1, rty2))


(** {1 Pretty printing} *)

let rec pp_ty (ppf : Format.formatter) (ty : ty) : unit =
  match force_ty ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty ppf ty =
  match ty with
  | Meta_var m -> pp_meta ppf m
  | Record_type rty -> pp_record_row_ty ppf rty
  | Variant_type rty -> pp_variant_row_ty ppf rty
  | Int_type -> Format.fprintf ppf "Int"
  | Bool_type -> Format.fprintf ppf "Bool"
  | ty -> Format.fprintf ppf "@[(%a)@]" pp_ty ty
and pp_meta ppf m =
  match !m with
  | Solved ty -> pp_atomic_ty ppf ty
  | Unsolved id -> Format.fprintf ppf "?%i" id
and pp_record_row_ty ppf rty =
  let pp_row =
    Format.pp_print_seq
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      (fun ppf (label, ty) -> Format.fprintf ppf "@[<2>@[%s@ :@]@ @[%a@]@]" label pp_ty ty)
  in
  match rty with
  | Row_entries row when Label_map.is_empty row ->
      Format.fprintf ppf "{}"
  | Row_entries row ->
      Format.fprintf ppf "@[{@ %a@ }@]" pp_row (Label_map.to_seq row)
  | Row_meta_var { contents = Unsolved_row (id, row) } ->
      Format.fprintf ppf "@[{@ ?%i..@ %a@ }@]" id pp_row (Label_map.to_seq row)
  | Row_meta_var { contents = Solved_row rty } ->
      pp_record_row_ty ppf rty
and pp_variant_row_ty ppf rty =
  let pp_row =
    Format.pp_print_seq
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ |@ ")
      (fun ppf (label, ty) -> Format.fprintf ppf "@[<2>@[%s@ :@]@ @[%a@]@]" label pp_ty ty)
  in
  match rty with
  | Row_entries row when Label_map.is_empty row ->
      Format.fprintf ppf "[|]"
  | Row_entries row ->
      Format.fprintf ppf "[%a]" pp_row (Label_map.to_seq row)
  | Row_meta_var { contents = Unsolved_row (id, row) } ->
      Format.fprintf ppf "[?%i..@ %a]" id pp_row (Label_map.to_seq row)
  | Row_meta_var { contents = Solved_row rty } ->
      pp_variant_row_ty ppf rty

let pp_name ppf name =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_name_ann ppf (name, ty) =
  Format.fprintf ppf "@[<2>@[%a :@]@ %a@]" pp_name name pp_ty ty

let pp_param ppf (name, ty) =
  Format.fprintf ppf "@[<2>(@[%a :@]@ %a)@]" pp_name name pp_ty ty

let rec pp_tm (names : name env) (ppf : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names ppf tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<v>%a@]" (go names) tm
  | Variant_elim (head, clauses) ->
      Format.fprintf ppf "@[<hv>@[match@ @[%a@]@ with@]@ %aend@]"
        (pp_app_tm names) head
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "")
          (fun ppf (label, (name, body)) ->
            Format.fprintf ppf "@[@[<2>@[|@ [%s@ :=@]@ @[%a@]]@]@ =>@ @[%a@]@]@ "
              label
              pp_name name
              (pp_tm (name :: names)) body))
        (Label_map.to_seq clauses)
  | Fun_lit (name, param_ty, body) ->
      let rec go names ppf tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Variant_lit (label, tm, ty) ->
      Format.fprintf ppf "@[<2>@[@[<2>@[[%s@ :=@]@ @[%a@]]@]@ :@]@ @[%a@]@]"
        label
        (pp_tm names) tm
        pp_ty ty
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names ppf tm
and pp_app_tm names ppf tm =
  match tm with
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_proj_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep ppf () = Format.fprintf ppf "@ " in
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_proj_tm names)) args
  | tm ->
      pp_proj_tm names ppf tm
and pp_proj_tm names ppf tm =
  match tm with
  | Record_proj (head, label) ->
      Format.fprintf ppf "%a.%s"
        (pp_proj_tm names) head
        label
  | tm ->
      pp_atomic_tm names ppf tm
and pp_atomic_tm names ppf tm =
  match tm with
  | Var index -> Format.fprintf ppf "%a" pp_name (List.nth names index)
  | Record_lit fields when Label_map.is_empty fields ->
      Format.fprintf ppf "{}"
  | Record_lit fields ->
      Format.fprintf ppf "@[{@ %a@ }@]"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (label, tm) ->
            Format.fprintf ppf "@[<2>@[%s@ :=@]@ @[%a@]@]" label (pp_tm names) tm))
        (Label_map.to_seq fields)
  | Int_lit i -> Format.fprintf ppf "%i" i
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | tm -> Format.fprintf ppf "@[(%a)@]" (pp_tm names) tm
