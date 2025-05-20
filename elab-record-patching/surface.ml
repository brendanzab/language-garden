(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core.Syntax}).
*)


(** Returns a list of duplicate elements in a list *)
let find_dupes (type a) (xs : a list) : a list =
  let rec go dupes = function
    | [] -> List.rev dupes
    | x :: xs when List.mem x xs && not (List.mem x dupes) -> go (x :: dupes) xs
    | _ :: xs -> go dupes xs in
  go [] xs


(** {1 Surface Syntax} *)

type pattern = string option

(** Terms in the surface language *)
type tm =
  | Let of pattern * params * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                                  (** References to named things: [ x ] *)
  | Ann of tm * tm                                  (** Terms annotated with types: [ x : A ] *)
  | Univ                                            (** Universe of types: [ Type ] *)
  | Fun_type of params * tm                         (** Function types: [ fun (x : A) -> B x ] *)
  | Fun_arrow of tm * tm                            (** Function arrow types: [ A -> B ] *)
  | Fun_lit of params * tm option * tm              (** Function literals: [ fun x => f x ] *)
  | Rec_type of (string * tm) list                  (** Record types: [ { x : A; ... } ]*)
  | Rec_lit of (string * (params * tm) option) list (** Record literals: [ { x := A; ... } ]*)
  | Rec_unit                                        (** Unit records: [ {} ] *)
  | Sing_type of tm * tm                            (** Singleton types: [ A [= x ] ] *)
  | App of tm * tm list                             (** Applications: [ f x ] *)
  | Proj of tm * string list                        (** Projections: [ r.l ] *)
  | Patch of tm * (string * tm) list                (** Record patches: [ R [ B := A; ... ] ] *)

(** We don’t need to add syntax for introducing and eliminating singletons in
    the surface language. These are instead added implicitly during
    elaboration to the core language. For example:

    - If we have [ x : Nat ] and [ x ≡ 7 : Nat ], we can elaborate the term
      [ x : A [= 7 ] ] into [ #sing-intro : A [= 7 ] ]
    - If we have [ x : Nat [= 7 ] ], we can elaborate the term [ x : Nat ]
      into [ 7 ]
*)

and param = pattern * tm option
and params = param list


(** {1 Elaboration } *)

(** This is where we implement user-facing type checking, in addition to
    translating the convenient surface language into a simpler, more explicit
    core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)

module Syntax = Core.Syntax
module Semantics = Core.Semantics


(** {2 Elaboration state} *)

(** The elaboration context records the bindings that are currently bound at
    the current scope in the program. The environments are unzipped to make it
    more efficient to call functions from {!Core.Semantics}. *)
type context = {
  size : Core.level;              (** Number of entries bound. *)
  names : Core.name Core.env;     (** Name environment *)
  tys : Semantics.vty Core.env;   (** Type environment *)
  tms : Semantics.vtm Core.env;   (** Term environment *)
}

(** The initial elaboration context, without any bindings *)
let initial_context = {
  size = 0;
  names = [];
  tys = [];
  tms = [];
}

(** Returns the next variable that will be bound in the context after calling
    {!bind_def} or {!bind_param} *)
let next_var (ctx : context) : Semantics.vtm =
  Semantics.Neu (Semantics.Var ctx.size)

(** Binds a definition in the context *)
let bind_def (ctx : context) (name : string option) (ty : Semantics.vty) (tm : Semantics.vtm) = {
  size = ctx.size + 1;
  names = name :: ctx.names;
  tys = ty :: ctx.tys;
  tms = tm :: ctx.tms;
}

(** Binds a parameter in the context *)
let bind_param (ctx : context) (name : string option) (ty : Semantics.vty) =
  bind_def ctx name ty (next_var ctx)

(** Lookup a name in the context *)
let lookup (ctx : context) (name : string) : (Core.index * Semantics.vty) option =
  (* Find the index of most recent binding in the context identified by
      [name], starting from the most recent binding. This gives us the
      corresponding de Bruijn index of the variable. *)
  ctx.names |> List.find_mapi @@ fun index name' ->
    match Some name = name' with
    | true -> Some (index, List.nth ctx.tys index)
    | false -> None

(** {3 Functions related to the core semantics} *)

(** These wrapper functions make it easier to call functions from the
    {!Core.Semantics} using state from the elaboration context. *)

let eval ctx : Syntax.tm -> Semantics.vtm =
  Semantics.eval ctx.tms
let quote ctx : Semantics.vtm -> Syntax.tm =
  Semantics.quote ctx.size
let normalise ctx : Syntax.tm -> Syntax.tm =
  Semantics.normalise ctx.size ctx.tms
let is_convertible ctx : Semantics.vtm -> Semantics.vtm -> bool =
  Semantics.is_convertible ctx.size
let pp ?(resugar = true) ctx : Format.formatter -> Syntax.tm -> unit =
  Syntax.pp ~resugar ctx.names


(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of string

(** Raises an {!Error} exception *)
let error message =
  raise (Error message)

let type_mismatch ctx ~expected ~found =
  Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %a@]@ @[found:    %a@]@]"
    (pp ctx) expected
    (pp ctx) found

let singleton_mismatch ctx ~expected ~found ~ty =
  Format.asprintf "@[<v 2>@[singleton mismatch@]@ @[expected: %a@]@ @[found:    %a@]@ @[type:     %a@]@]"
    (pp ctx) expected
    (pp ctx) found
    (pp ctx) ty

let field_mismatch ~expected ~found =
  Format.asprintf "@[<v 2>@[field mismatch@]@ @[expected label: `%s`@]@ @[found label:    `%s`@]@]"
    expected
    found

let missing_field (label : string) =
  Format.asprintf "field with label `%s` not found in record" label

let not_bound (name : string) =
  Format.asprintf "`%s` is not bound in the current scope" name

let ambiguous_param (name : string option) =
    Format.asprintf "ambiguous function parameter `%s`"
      (Option.value ~default:"_" name)


(** {2 Coercive subtyping} *)

(** Returns a coercion from a term of one type to a term of another type. By
    performing coercions during elaboration we avoid having to introduce
    subtyping in the core language. *)
let rec coerce (ctx : context) (from_ty : Semantics.vty) (to_ty : Semantics.vty) (tm : Syntax.tm) : Syntax.tm =
  (* TODO: Return [tm] unchanged if no coercion was needed, avoiding unnecessary
    eta-expansions to the elaborated terms. An example of this can be seen here:
    https://github.com/AndrasKovacs/staged/blob/9e381eb162f44912d70fb843c4ca6567b0d1683a/demo/Elaboration.hs#L87-L140 *)

  match from_ty, to_ty with
  (* No need to coerce the term if both types are already the same! *)
  | from_ty, to_ty when is_convertible ctx from_ty to_ty -> tm
  (* Coerce the term to a singleton with {!Syntax.Sing_intro}, if the term is
    convertible to the term expected by the singleton *)
  | from_ty, Semantics.Sing_type (to_ty, sing_tm) ->
      let tm = coerce ctx from_ty to_ty tm in
      let tm' = eval ctx tm in
      if is_convertible ctx sing_tm tm' then Syntax.Sing_intro else
        error (singleton_mismatch ctx
          ~expected:(quote ctx sing_tm)
          ~found:(quote ctx tm')
          ~ty:(quote ctx to_ty))
  (* Coerce the singleton back to its underlying term with {!Syntax.Sing_elim}
    and attempt further coercions from its underlying type *)
  | Semantics.Sing_type (from_ty, sing_tm), to_ty ->
      coerce ctx from_ty to_ty (quote ctx sing_tm)
  (* Coerce the fields of a record with record eta expansion *)
  | Semantics.Rec_type from_decls, Semantics.Rec_type to_decls ->
      (* TODO: bind [tm] to a local variable to avoid duplicating records *)
      let rec go from_decls to_decls =
        match from_decls, to_decls with
        | Semantics.Nil, Semantics.Nil -> []
        (* Use eta-expansion to coerce fields that share the same label *)
        | Semantics.Cons (from_label, from_ty, from_decls)
        , Semantics.Cons (to_label, to_ty, to_decls) when from_label = to_label ->
            let from_tm = eval ctx (Syntax.Rec_proj (tm, from_label)) in
            let to_tm = coerce ctx from_ty to_ty (Syntax.Rec_proj (tm, from_label)) in
            (to_label, to_tm) :: go (from_decls from_tm) (to_decls (eval ctx to_tm))
        (* When the type of the target field is a singleton we can use it to
            fill in the definition of a missing field in the source term. This
            is similar to how we handle missing fields in {!check}. *)
        | from_decls, Semantics.Cons (to_label, Semantics.Sing_type (_, _), to_decls) ->
            let to_tm = Syntax.Sing_intro in
            (to_label, to_tm) :: go from_decls (to_decls (eval ctx to_tm))
        | Semantics.Cons (from_label, _, _), Semantics.Cons (to_label, _, _) ->
            error (field_mismatch ~expected:to_label ~found:from_label)
        | _, _ -> Semantics.error "mismatched telescope length"
      in
      Syntax.Rec_lit (go from_decls to_decls)
  (* TODO: subtyping for functions! *)
  | from_ty, to_ty  ->
      error (type_mismatch ctx
        ~expected:(quote ctx to_ty)
        ~found:(quote ctx from_ty))


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors, and provide enough {i control} to the
    algorithm to keep type inference deciable even in the presence of ‘fancy’
    types, for example dependent types, higher rank types, and subtyping. *)

(** Elaborate a term in the surface language into a term in the core language
    in the presence of a type annotation. *)
let rec check (ctx : context) (tm : tm) (expected_ty : Semantics.vty) : Syntax.tm =
  match tm, expected_ty with
  (* Let expressions *)
  | Let (name, params, def_ty, def, body), expected_ty ->
      let def, def_ty = infer_fun_lit ctx params def_ty def in
      let def_ty' = eval ctx def_ty in
      let body = check (bind_def ctx name def_ty' (eval ctx def)) body expected_ty in
      Syntax.Let (name, def_ty, def, body)

  (* Function literals *)
  | Fun_lit (params, body_ty, body), expected_ty ->
      check_fun_lit ctx params body_ty body expected_ty

  (* Record literals *)
  | Rec_lit defns, Semantics.Rec_type decls ->
      (* TODO: elaborate fields out of order? *)
      let rec go defns decls =
        match defns, decls with
        | [], Semantics.Nil -> []
        (* When the labels match, check the term against the type, handling
           punned fields appropriately. *)
        | (label, tm) :: defns, Semantics.Cons (label', ty, decls) when label = label' ->
            let tm = match tm with
              | Some (params, tm) -> check_fun_lit ctx params None tm ty (* explicit field definition *)
              | None -> check ctx (Name label) ty (* punned field definition *)
            in
            (label, tm) :: go defns (decls (eval ctx tm))
        (* When the expected type of a field is a singleton we can use it to
            fill in the definition of a missing fields in the record literal. *)
        | defns, Semantics.Cons (label, Semantics.Sing_type (_, _), decls) ->
            let tm = Syntax.Sing_intro in
            (label, tm) :: go defns (decls (eval ctx tm))
        | _, Semantics.Cons (label, _, _) -> error (missing_field label)
        | (label, _) :: _, Semantics.Nil -> error ("unexpected field `" ^ label ^ "` in record literal")
      in
      Syntax.Rec_lit (go defns decls)

  (* Records with no entries. These need to be disambiguated with a type
      annotation. *)
  | Rec_unit, Semantics.Univ ->
      Syntax.Rec_type []
  | Rec_unit, Semantics.Rec_type Semantics.Nil ->
      Syntax.Rec_lit []

  (* Singleton introduction. No need for any syntax in the surface language
      here, instead we use the type annotation to drive this. *)
  | tm, Semantics.Sing_type (ty, sing_tm) ->
      let tm = check ctx tm ty in
      let tm' = eval ctx tm in
      if is_convertible ctx sing_tm tm' then Syntax.Sing_intro else
        error (singleton_mismatch ctx
          ~expected:(quote ctx sing_tm)
          ~found:(quote ctx tm')
          ~ty:(quote ctx ty))

  (* For anything else, try inferring the type of the term, then attempting to
      coerce the term to the expected type. *)
  | tm, expected_ty ->
      let tm, ty' = infer ctx tm in
      let tm, ty' = elim_implicits ctx tm ty' in
      coerce ctx ty' expected_ty tm

(** Elaborate a term in the surface language into a term in the core language,
    inferring its type. *)
and infer (ctx : context) (tm : tm) : Syntax.tm * Semantics.vty =
  match tm with
  (* Let expressions *)
  | Let (name, params, def_ty, def, body) ->
      let def, def_ty = infer_fun_lit ctx params def_ty def in
      let def_ty' = eval ctx def_ty in
      let body, body_ty = infer (bind_def ctx name def_ty' (eval ctx def)) body in
      Syntax.Let (name, def_ty, def, body), body_ty

  (* Named terms *)
  | Name name ->
      begin match lookup ctx name with
      | Some (index, vty) -> (Syntax.Var index, vty)
      | None -> error (not_bound name)
      end

  (* Annotated terms *)
  | Ann (tm, ty) ->
      let ty = check ctx ty Semantics.Univ in
      let ty' = eval ctx ty in
      let tm = check ctx tm ty' in
      tm, ty'

    (* Universes *)
  | Univ ->
      (* We use [Type : Type] here for simplicity, which means this type
          theory is inconsistent. This is okay for a toy type system, but we’d
          want look into using universe levels in an actual implementation. *)
      Syntax.Univ, Semantics.Univ

  (* Function types *)
  | Fun_type (params, body_ty) ->
      let rec go ctx = function
        | [] -> check ctx body_ty Semantics.Univ
        (* Function types always require annotations *)
        | (name, None) :: _ -> error (ambiguous_param name)
        | (name, Some param_ty) :: params ->
            let param_ty = check ctx param_ty Semantics.Univ in
            let ctx = bind_param ctx name (eval ctx param_ty) in
            Syntax.Fun_type (name, param_ty, go ctx params)
      in
      go ctx params, Semantics.Univ

  (* Arrow types. These are implemented as syntactic sugar for non-dependent
      function types. *)
  | Fun_arrow (param_ty, body_ty) ->
      let param_ty = check ctx param_ty Semantics.Univ in
      let ctx = bind_param ctx None (eval ctx param_ty) in
      let body_ty = check ctx body_ty Semantics.Univ in
      Syntax.Fun_type (None, param_ty, body_ty), Semantics.Univ

  (* Function literals. These do not have type annotations on their arguments
      and so we don’t know ahead of time what types to use for the arguments
      when adding them to the context. As a result with coose to throw an
      ambiguity error here. *)
  | Fun_lit (params, body_ty, body) ->
      let fun_tm, fun_ty = infer_fun_lit ctx params body_ty body in
      fun_tm, eval ctx fun_ty

  (* Function application *)
  | Rec_type decls ->
      let rec go ctx seen_labels = function
        | [] -> []
        | (label, _) :: _ when List.mem label seen_labels ->
            error ("duplicate label `" ^ label ^ "` in record type")
        | (label, ty) :: decls ->
            let ty = check ctx ty Semantics.Univ in
            let ctx = bind_param ctx (Some label) (eval ctx ty) in
            (label, ty) :: go ctx (label :: seen_labels) decls
      in
      Syntax.Rec_type (go ctx [] decls), Semantics.Univ

  (* Unit records. These are ambiguous in inference mode. We could default to
      one or the other, and perhaps coerce between them, but we choose just to
      throw an error instead. *)
  | Rec_lit _ -> error "ambiguous record literal"
  | Rec_unit -> error "ambiguous unit record"

  (* Singleton types *)
  | Sing_type (ty, sing_tm) ->
      let ty = check ctx ty Semantics.Univ in
      let sing_tm = check ctx sing_tm (eval ctx ty) in
      Syntax.Sing_type (ty, sing_tm), Semantics.Univ

  (* Application *)
  | App (head, args) ->
      List.fold_left
        (fun (head, head_ty) arg ->
          match elim_implicits ctx head head_ty with
          | head, Semantics.Fun_type (_, param_ty, body_ty) ->
              let arg = check ctx arg param_ty in
              Syntax.Fun_app (head, arg), body_ty (eval ctx arg)
          | _ -> error "not a function")
        (infer ctx head)
        args

  (* Field projection *)
  | Proj (head, labels) ->
      List.fold_left
        (fun (head, head_ty) label ->
          match elim_implicits ctx head head_ty with
          | head, Semantics.Rec_type decls ->
              begin match Semantics.proj_ty (eval ctx head) decls label with
              | Some ty -> Syntax.Rec_proj (head, label), ty
              | None -> error (missing_field label)
              end
          | _ -> error "not a record")
        (infer ctx head)
        labels

  (* Patches. Here we add patches to record types by creating a copy of the
      type with singletons in place of the patched fields  *)
  | Patch (head, patches) ->
      let rec go ctx decls patches =
        match decls, patches with
        | Semantics.Nil, [] -> []
        | Semantics.Nil, (label, _) :: _ ->
            error ("field `" ^ label ^ "` not found in record type")
        | Semantics.Cons (label, ty, tys), patches ->
            let ty' = quote ctx ty in
            begin match List.assoc_opt label patches with
            | Some patch_tm ->
                let tm = check ctx patch_tm ty in
                let tm' = eval ctx tm in
                let ctx = bind_def ctx (Some label) (Semantics.Sing_type (ty, tm')) tm' in
                let patches = List.remove_assoc label patches in
                (label, Syntax.Sing_type (ty', tm)) :: go ctx (tys tm') patches
            | None ->
                let var = next_var ctx in
                let ctx = bind_def ctx (Some label) ty var in
                (label, ty') :: go ctx (tys var) patches
            end
      in

      let dupes = find_dupes (List.map fst patches) in
      if List.compare_length_with dupes 0 <> 0 then
        error ("duplicate labels in patches: `" ^ String.concat "`, `" dupes ^ "`")
      else
        let head = check ctx head Semantics.Univ in
        begin match eval ctx head with
        | Semantics.Rec_type decls ->
            let decls = go ctx decls patches in
            Syntax.Rec_type decls, Semantics.Univ
        | _ -> error "can only patch record types"
        end

(** Elaborate a function literal in checking mode. *)
and check_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) (expected_ty : Semantics.vty) =
  match params, body_ty, expected_ty with
  | [], None, expected_ty -> check ctx body expected_ty
  | [], Some body_ty, expected_ty ->
      let body_ty = check ctx body_ty Semantics.Univ in
      let body_ty' = eval ctx body_ty in
      if is_convertible ctx body_ty' expected_ty then
        check ctx body body_ty'
      else error (type_mismatch ctx
        ~expected:(quote ctx expected_ty)
        ~found:body_ty)
  | (name, param_ty) :: params, body_ty, Semantics.Fun_type (_, expected_param_ty, expected_body_ty) ->
      let var = next_var ctx in
      let param_ty =
        match param_ty with
        | None -> expected_param_ty
        | Some param_ty ->
            let param_ty = check ctx param_ty Semantics.Univ in
            let param_ty' = eval ctx param_ty in
            let expected_param_ty = expected_param_ty in
            (* Check that the parameter annotation in the function literal
                matches the expected parameter type. *)
            if is_convertible ctx param_ty' expected_param_ty then param_ty' else
              error (type_mismatch ctx
                ~expected:(quote ctx expected_param_ty)
                ~found:param_ty)
      in
      let ctx = bind_def ctx name param_ty var in
      let body = check_fun_lit ctx params body_ty body (expected_body_ty var) in
      Syntax.Fun_lit (name, body)
  | _, _, _ ->
      error "too many parameters in function literal"

(** Elaborate a function literal in inference mode. *)
and infer_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) =
  match params, body_ty with
  | [], None ->
      let body, body_ty = infer ctx body in
      body, quote ctx body_ty
  | [], Some body_ty ->
      let body_ty = check ctx body_ty Semantics.Univ in
      check ctx body (eval ctx body_ty), body_ty
  | (name, param_ty) :: params, body_ty ->
      let var = next_var ctx in
      let param_ty =
        match param_ty with
        (* We’re in inference mode, so function parameters need annotations *)
        | None -> error (ambiguous_param name)
        | Some param_ty -> check ctx param_ty Semantics.Univ
      in
      let ctx = bind_def ctx name (eval ctx param_ty) var in
      let body, body_ty = infer_fun_lit ctx params body_ty body in
      Syntax.Fun_lit (name, body), Syntax.Fun_type (name, param_ty, body_ty)


(** {2 Eliminating implicit connectives} *)

(** Connectives that were introduced implicitly during elaboration can
    sometimes get in the way, for example when calling {!coerce}, or when
    elaborating the head of an elimination. This removes them by applying
    appropriate elimination forms. *)
and elim_implicits (ctx : context) (tm : Syntax.tm) (ty : Semantics.vty) : Syntax.tm * Semantics.vty =
  match ty with
  (* Eliminate the singleton, converting it back to its underlying term *)
  | Sing_type (ty, sing_tm) ->
      elim_implicits ctx (quote ctx sing_tm) ty
  (* TODO: we can eliminate implicit functions here. See the elaboration-zoo
    for ideas on how to do this: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L48-L53 *)
  | ty -> tm, ty
