(** {0 Core language} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option

(** {i De Bruijn index} that represents a variable occurrence by the number of
    binders between the occurrence and the binder it refers to. *)
type index = int

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Labels} *)

(** Labels are significant to the equality of terms. They are typically used
    to distinguish elements in variants, records, etc. *)
type label = string

(** An unordered row of elements distinguished by label. *)
module Label_map = Map.Make (String)


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

(** Row type syntax *)
and row_ty =
  | Row_meta_var of row_meta
  | Row_entries of ty Label_map.t

(** The current state of a metavariable *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id

(** The current state of a row metavariable *)
and row_meta_state =
  | Solved_row of row_ty
  | Unsolved_row of row_meta_id * ty Label_map.t

(** Mutable representation of metavariables. These are updated in-place during
    unification and when types are forced. Alternatively we could have
    chosen to store these in a separate metacontext, like in the
    elaboration-zoo. *)
and meta = meta_state ref

(* Mutable representation of row metavariables. *)
and row_meta = row_meta_state ref

(** Term syntax *)
type tm =
  | Var of index
  | Prim of Prim.t
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

(** Apply a term to a list of arguments *)
let fun_app (head : tm) (args : tm list) : tm =
  List.fold_left (fun head arg -> Fun_app (head, arg)) head args


module Semantics = struct

  (** {1 Values} *)

  (** Evaluated terms *)
  type vtm =
    | Prim_app of Prim.t * vtm list
    | Fun_lit of clos
    | Record_lit of vtm Label_map.t
    | Variant_lit of label * vtm
    | Int_lit of int
    | Bool_lit of bool

  (** Closures that can be instantiated with a value. The environment provides
      a value for each variable in the term, except for the variable that the
      closure will be instantiated with during evaluation. *)
  and clos = vtm env * tm


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (_, _, body) ->
        Fun_lit (env, body)
    | Fun_app (head, arg) ->
        begin match eval env head with
        | Fun_lit clos -> clos_app clos (eval env arg)
        | Prim_app (prim, args) -> prim_app prim ((eval env arg) :: args)
        | _ -> invalid_arg "expected function"
        end
    | Record_lit fields ->
        Record_lit (fields |> Label_map.map (eval env))
    | Record_proj (head, label) ->
        begin match eval env head with
        | Record_lit fields -> Label_map.find label fields
        | _ -> invalid_arg "expected record"
        end
    | Variant_lit (label, tm, _) ->
        Variant_lit (label, eval env tm)
    | Variant_elim (head, clauses) ->
        begin match eval env head with
        | Variant_lit (label, vtm) ->
            let _, body = Label_map.find label clauses in
            eval (vtm :: env) body
        | _ -> invalid_arg "expected variant"
        end
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        begin match eval env head with
        | Bool_lit true -> eval env tm1
        | Bool_lit false -> eval env tm2
        | _ -> invalid_arg "expected boolean"
        end

  and clos_app (env, body : clos) (arg : vtm) : vtm =
    eval (arg :: env) body

  and prim_app (prim : Prim.t) (args : vtm list) : vtm =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Prim_app (prim, args)


  (** {1 Pretty printing} *)

  let pp_vtm : vtm -> Format.formatter -> unit =
    let pp_trailing_semi : Format.formatter -> unit =
      Format.pp_print_custom_break ~fits:(" ", 0, "") ~breaks:(";", 0, "")
    in
    let rec pp_vtm vtm ppf =
      match vtm with
      | Variant_lit (label, vtm) ->
          Format.fprintf ppf "@[<2>@[[%s@ :=@]@ @[%t@]]@]"
            label
            (pp_vtm vtm)
      | vtm ->
          pp_app_vtm vtm ppf
    and pp_app_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, ((_ :: _) as args)) ->
          let pp_sep ppf () = Format.fprintf ppf "@ " in
          Format.fprintf ppf "@[<2>#%s %t@]"
            (Prim.name prim)
            (Fun.flip (Format.pp_print_list (Fun.flip pp_atomic_vtm) ~pp_sep) args)
      | vtm ->
          pp_atomic_vtm vtm ppf
    and pp_atomic_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, []) -> Format.fprintf ppf "#%s" (Prim.name prim)
      | Fun_lit _ -> Format.fprintf ppf "<function>"
      | Record_lit fields when Label_map.is_empty fields ->
          Format.fprintf ppf "{}"
      | Record_lit fields ->
          Format.fprintf ppf "@[{%a%t}@]"
            (Format.pp_print_seq
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ";")
              (fun ppf (label, tm) ->
                Format.fprintf ppf "@;<1 2>@[<2>@[%s@ :=@]@ @[%t@]@]" label (pp_vtm tm)))
            (Label_map.to_seq fields)
            pp_trailing_semi
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Variant_lit _ | Prim_app (_, (_ :: _)) as vtm ->
          Format.fprintf ppf "@[(%t)@]" (pp_vtm vtm)
    in
    pp_vtm

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
  match force_row_ty rty1, force_row_ty rty2 with
  | Row_meta_var rm1, Row_meta_var rm2 when rm1 == rm2 -> ()

  (* Should never occur *)
  | Row_meta_var { contents = Solved_row _ }, _
  | _, Row_meta_var { contents = Solved_row _ } ->
      failwith "expected forced row type"

  (* Unify two unsolved rows *)
  | Row_meta_var ({ contents = Unsolved_row (id, row1) } as rm1),
    (Row_meta_var ({ contents = Unsolved_row (_, row2) } as rm2) as rty) ->
      occurs_row_ty rm1 rty;
      let merge_rows =
        Label_map.merge @@ fun _ ty1 ty2 ->
          match ty1, ty2 with
          | Some ty1, Some ty2 -> unify_tys ty1 ty2; Some ty1
          | Some ty, None | None, Some ty -> Some ty
          | None, None  -> None
      in
      rm2 := Unsolved_row (id, merge_rows row1 row2);
      rm1 := Solved_row rty

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

  (* Unify concrete row types *)
  | Row_entries row1, Row_entries row2 ->
      if not (Label_map.equal (fun ty1 ty2 -> unify_tys ty1 ty2; true) row1 row2) then
        raise (Mismatched_row_types (rty1, rty2))


(** {1 Pretty printing} *)

let pp_trailing_semi : Format.formatter -> unit =
  Format.pp_print_custom_break ~fits:(" ", 0, "") ~breaks:(";", 0, "")

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty ppf =
    match ty with
    | Meta_var m -> pp_meta pp_ty m ppf
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty ppf
  and pp_atomic_ty ty ppf =
    match ty with
    | Meta_var m -> pp_meta pp_atomic_ty m ppf
    | Record_type rty -> pp_record_row_ty rty ppf
    | Variant_type rty -> pp_variant_row_ty rty ppf
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)

  and pp_meta pp_ty m ppf =
    match !m with
    | Solved ty -> pp_ty ty ppf
    | Unsolved id -> Format.fprintf ppf "?%i" id

  and pp_record_row_ty rty ppf =
    let pp_row row ppf =
      Format.pp_print_seq
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";")
        (fun ppf (label, ty) -> Format.fprintf ppf "@;<1 2>@[<2>@[%s@ :@]@ @[%t@]@]" label (pp_ty ty))
        ppf row
    in
    match rty with
    | Row_entries row when Label_map.is_empty row ->
        Format.fprintf ppf "{}"
    | Row_entries row ->
        Format.fprintf ppf "@[{%t%t}@]" (pp_row (Label_map.to_seq row)) pp_trailing_semi
    | Row_meta_var { contents = Unsolved_row (id, row) } ->
        Format.fprintf ppf "@[{@ ?%i..%t%t}@]" id (pp_row (Label_map.to_seq row)) pp_trailing_semi
    | Row_meta_var { contents = Solved_row rty } ->
        pp_record_row_ty rty ppf

  and pp_variant_row_ty rty ppf =
    let pp_row row ppf =
      Format.pp_print_seq
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ |@ ")
        (fun ppf (label, ty) -> Format.fprintf ppf "@[<2>@[%s@ :@]@ @[%t@]@]" label (pp_ty ty))
        ppf row
    in
    match rty with
    | Row_entries row when Label_map.is_empty row ->
        Format.fprintf ppf "[|]"
    | Row_entries row ->
        Format.fprintf ppf "[%t]" (pp_row (Label_map.to_seq row))
    | Row_meta_var { contents = Unsolved_row (id, row) } ->
        Format.fprintf ppf "[?%i..@ %t]" id (pp_row (Label_map.to_seq row))
    | Row_meta_var { contents = Solved_row rty } ->
        pp_variant_row_ty rty ppf
  in

  pp_ty

let pp_name (name : name) (ppf : Format.formatter) : unit =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_name_ann (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (pp_ty ty)

let pp_param (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (pp_ty ty)

let pp_tm : name env -> tm -> Format.formatter -> unit =
  let rec pp_tm names tm ppf =
    match tm with
    | Let _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names tm)
    | Variant_elim (head, clauses) ->
        Format.fprintf ppf "@[<hv>@[match@ @[%t@]@ with@]@ %aend@]"
          (pp_app_tm names head)
          (Format.pp_print_seq
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "")
            (fun ppf (label, (name, body)) ->
              Format.fprintf ppf "@[@[<2>@[|@ [%s@ :=@]@ @[%t@]]@]@ =>@ @[%t@]@]@ "
                label
                (pp_name name)
                (pp_tm (name :: names) body)))
          (Label_map.to_seq clauses)
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
    | Variant_lit (label, tm, ty) ->
        Format.fprintf ppf "@[<2>@[@[<2>@[[%s@ :=@]@ @[%t@]]@]@ :@]@ @[%t@]@]"
          label
          (pp_tm names tm)
          (pp_ty ty)
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
      | tm ->
          pp_proj_tm names tm ppf
    in
    match tm with
    | Fun_app _ as tm ->
        Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
    | tm ->
        pp_proj_tm names tm ppf
  and pp_proj_tm names tm ppf =
    let rec go tm ppf =
      match tm with
      | Record_proj (head, label) ->
            Format.fprintf ppf "%t@,.%s" (go head) label
      | tm ->
          pp_atomic_tm names tm ppf
    in
    match tm with
    | Record_proj _ as tm ->
        Format.fprintf ppf "@[<2>%t@]" (go tm)
    | tm ->
        pp_atomic_tm names tm ppf
  and pp_atomic_tm names tm ppf =
    match tm with
    | Var index -> Format.fprintf ppf "%t" (pp_name (List.nth names index))
    | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
    | Record_lit fields when Label_map.is_empty fields ->
        Format.fprintf ppf "{}"
    | Record_lit fields ->
        Format.fprintf ppf "@[{%a%t}@]"
          (Format.pp_print_seq
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ";")
            (fun ppf (label, tm) ->
              Format.fprintf ppf "@;<1 2>@[<2>@[%s@ :=@]@ @[%t@]@]" label (pp_tm names tm)))
          (Label_map.to_seq fields)
          pp_trailing_semi
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Fun_lit _ | Fun_app _ | Record_proj _ | Variant_lit _
    | Variant_elim _ | Bool_elim _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm
