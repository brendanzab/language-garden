(** {0 Surface language} *)

(** The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar, subtyping, and other higher level language
    features that make programming more convenient (in comparison to the
    {!Core.Syntax}). *)


open Base


(** {1 Surface Syntax} *)

type pattern = string option

(** Terms in the surface language *)
type tm =
  | Let of pattern * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                         (** References to named things: [ x ] *)
  | Ann of tm * tm                         (** Terms annotated with types: [ x : A ] *)
  | Univ                                   (** Universe of types: [ Type ] *)
  | FunType of (pattern * tm) list * tm    (** Function types: [ fun (x : A) -> B x ] *)
  | FunArrow of tm * tm                    (** Function arrow types: [ A -> B ] *)
  | FunLit of pattern list * tm            (** Function literals: [ fun x := f x ] *)
  | RecType of (string * tm) list          (** Record types: [ { x : A; ... } ]*)
  | RecLit of (string * tm option) list    (** Record literals: [ { x := A; ... } ]*)
  | RecUnit                                (** Unit records: [ {} ] *)
  | SingType of tm * tm                    (** Singleton types: [ A [= x ] ] *)
  | App of tm * tm list                    (** Applications: [ f x ] *)
  | Proj of tm * string list               (** Projections: [ r.l ] *)
  | Patch of tm * (string * tm) list       (** Record patches: [ R [ B := A; ... ] ] *)

(** We don’t need to add syntax for introducing and eliminating singletons in
    the surface language. These are instead added implicitly during
    elaboration to the core language. For example:

    - If we have [ x : Nat ] and [ x ≡ 7 : Nat ], we can elaborate the term
      [ x : A [= 7 ] ] into [ #sing-intro x : A [= 7 ] ]
    - If we have [ x : Nat [= 7 ] ], we can elaborate the term [ x : Nat ]
      into [ #sing-elim x 7 : Nat ]
*)


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
let next_var context =
  Semantics.Neu (Semantics.Var context.size)

(** Binds a definition in the context *)
let bind_def context name ty tm = {
  size = context.size + 1;
  names = name :: context.names;
  tys = ty :: context.tys;
  tms = tm :: context.tms;
}

(** Binds a parameter in the context *)
let bind_param context name ty =
  bind_def context name ty (next_var context)

(** {3 Functions related to the core semantics} *)

(** These wrapper functions make it easier to call functions from the
    {!Core.Semantics} using state from the elaboration context. *)

let eval context : Syntax.tm -> Semantics.vtm =
  Semantics.eval context.tms
let quote context : Semantics.vtm -> Semantics.vty -> Syntax.tm =
  Semantics.quote context.size context.tys
let normalise context : Syntax.tm -> Semantics.vtm -> Syntax.tm =
  Semantics.normalise context.size context.tms context.tys
let is_convertible context : Semantics.vtm -> Semantics.vtm -> Semantics.vty -> bool =
  Semantics.is_convertible context.size context.tys
let pp ?(resugar = true) context : Format.formatter -> Syntax.tm -> unit =
  Syntax.pp ~resugar context.names


(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors or a missing type annotations. This is
    normal, and should be rendered nicely to the programmer. *)
exception Error of string

(** Raises an {!Error} exception *)
let error message =
  raise (Error message)

let type_mismatch context ~expected ~found =
  Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %a@]@ @[found:    %a@]@]"
    (pp context) expected
    (pp context) found

let singleton_mismatch context ~expected ~found ~ty =
  Format.asprintf "@[<v 2>@[singleton mismatch@]@ @[expected: %a@]@ @[found:    %a@]@ @[type:     %a@]@]"
    (pp context) expected
    (pp context) found
    (pp context) ty

let field_mismatch ~expected ~found =
  Format.asprintf "@[<v 2>@[field mismatch@]@ @[expected label: `%s`@]@ @[found label:    `%s`@]@]"
    expected
    found

let missing_field label =
  Format.asprintf "field with label `%s` not found in record" label

let not_bound name =
  Format.asprintf "`%s` is not bound in the current scope" name

let ambiguous_param name =
    Format.asprintf "ambiguous function parameter `%s`"
      (Option.value ~default:"_" name)


(** {2 Coercive subtyping} *)

(** Returns a coercion from a term of one type to a term of another type. By
    performing coercions during elaboration we avoid having to introduce
    subtyping in the core language. *)
let rec coerce context from_ty to_ty tm : Syntax.tm =
  match from_ty, to_ty with
  (* No need to coerce the term if both types are already the same! *)
  | from_ty, to_ty when is_convertible context from_ty to_ty Semantics.Univ -> tm
  (* Coerce the term to a singleton with {!Syntax.SingIntro}, if the term is
    convertible to the term expected by the singleton *)
  | from_ty, Semantics.SingType (to_ty, sing_tm) ->
      let tm = coerce context from_ty to_ty tm in
      let tm' = eval context tm in
      if is_convertible context sing_tm tm' to_ty then Syntax.SingIntro tm else
        error (singleton_mismatch context
          ~expected:(quote context sing_tm to_ty)
          ~found:(quote context tm' from_ty)
          ~ty:(quote context to_ty Semantics.Univ))
  (* Coerce the singleton back to its underlying term with {!Syntax.SingElim}
    and attempt further coercions from its underlying type *)
  | Semantics.SingType (from_ty, sing_tm), to_ty ->
      let tm = Syntax.SingElim (tm, quote context sing_tm from_ty) in
      coerce context from_ty to_ty tm
  (* Coerce the fields of a record with record eta expansion *)
  | Semantics.RecType from_decls, Semantics.RecType to_decls ->
      let rec go from_decls to_decls =
        match from_decls, to_decls with
        | Semantics.Nil, Semantics.Nil -> []
        (* Use eta-expansion to coerce fields that share the same label *)
        | Semantics.Cons (from_label, from_ty, from_decls)
        , Semantics.Cons (to_label, to_ty, to_decls) when from_label = to_label ->
            let from_tm = eval context (Syntax.RecProj (tm, from_label)) in
            let to_tm = coerce context from_ty to_ty (Syntax.RecProj (tm, from_label)) in
            (to_label, to_tm) :: go (from_decls from_tm) (to_decls (eval context to_tm))
        (* When the type of the target field is a singleton we can use it to
            fill in the definition of a missing field in the source term. This
            is similar to how we handle missing fields in {!check}. *)
        | from_decls, Semantics.Cons (to_label, Semantics.SingType (to_ty, sing_tm), to_decls) ->
            let to_tm = Syntax.SingIntro (quote context sing_tm to_ty) in
            (to_label, to_tm) :: go from_decls (to_decls (eval context to_tm))
        | Semantics.Cons (from_label, _, _), Semantics.Cons (to_label, _, _) ->
            error (field_mismatch ~expected:to_label ~found:from_label)
        | _, _ -> Semantics.error "mismatched telescope length"
      in
      Syntax.RecLit (go from_decls to_decls)
  (* TODO: subtyping for functions! *)
  | from_ty, to_ty  ->
      error (type_mismatch context
        ~expected:(quote context to_ty Semantics.Univ)
        ~found:(quote context from_ty Semantics.Univ))


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors, and provide enough {i control} to the
    algorithm to keep type inference deciable even in the presence of ‘fancy’
    types, for example dependent types, higher rank types, and subtyping. *)

(** Elaborate a term in the surface language into a term in the core language
    in the presence of a type annotation. *)
let rec check context tm ty : Syntax.tm =
  match tm, ty with
  (* Let expressions *)
  | Let (name, def_ty, def, body), ty ->
      let def, def_ty =
        match def_ty with
        | None -> infer context def
        | Some def_ty ->
            let def_ty = check context def_ty Semantics.Univ in
            let def_ty' = eval context def_ty in
            let def = check context def def_ty' in
            (Syntax.Ann (def, def_ty), def_ty')
      in
      let context = bind_def context name def_ty (eval context def) in
      Syntax.Let (name, def, check context body ty)

  (* Function literals *)
  | FunLit (names, body), ty ->
      (* Iterate over the parameters of the function literal, constructing a
          function literal in the core language. *)
      let rec go context names ty =
        match names, ty with
        | [], body_ty -> check context body body_ty
        | name :: names, Semantics.FunType (_, param_ty, body_ty) ->
            let var = next_var context in
            let context = bind_def context name param_ty var in
            Syntax.FunLit (name, go context names (body_ty var))
        | _, _ -> error "too many parameters in function literal"
      in
      go context names ty

  (* Record literals *)
  | RecLit defns, Semantics.RecType decls ->
      (* TODO: elaborate fields out of order? *)
      let rec go defns decls =
        match defns, decls with
        | [], Semantics.Nil -> []
        (* When the labels match, check the term against the type, handling
           punned fields appropriately. *)
        | (label, tm) :: defns, Semantics.Cons (label', ty, decls) when label = label' ->
            let tm = match tm with
              | Some tm -> check context tm ty (* explicit field definition *)
              | None -> check context (Name label) ty (* punned field definition *)
            in
            (label, tm) :: go defns (decls (eval context tm))
        (* When the expected type of a field is a singleton we can use it to
            fill in the definition of a missing fields in the record literal. *)
        | defns, Semantics.Cons (label, Semantics.SingType (ty, sing_tm), decls) ->
            let tm = Syntax.SingIntro (quote context sing_tm ty) in
            (label, tm) :: go defns (decls (eval context tm))
        | _, Semantics.Cons (label, _, _) -> error (missing_field label)
        | (label, _) :: _, Semantics.Nil -> error ("unexpected field `" ^ label ^ "` in record literal")
      in
      Syntax.RecLit (go defns decls)

  (* Records with no entries. These need to be disambiguated with a type
      annotation. *)
  | RecUnit, Semantics.Univ ->
      Syntax.RecType Syntax.Nil
  | RecUnit, Semantics.RecType Semantics.Nil ->
      Syntax.RecLit []

  (* Singleton introduction. No need for any syntax in the surface language
      here, instead we use the type annotation to drive this. *)
  | tm, Semantics.SingType (ty, sing_tm) ->
      let tm = check context tm ty in
      let tm' = eval context tm in
      if is_convertible context sing_tm tm' ty then Syntax.SingIntro tm else
        error (singleton_mismatch context
          ~expected:(quote context sing_tm ty)
          ~found:(quote context tm' ty)
          ~ty:(quote context ty Semantics.Univ))

  (* For anything else, try inferring the type of the term, then attempting to
      coerce the term to the expected type. *)
  | tm, ty ->
      let tm, ty' = infer context tm in
      let tm, ty' = elim_implicits context tm ty' in
      coerce context ty' ty tm

(** Elaborate a term in the surface language into a term in the core language,
    inferring its type. *)
and infer context : tm -> Syntax.tm * Semantics.vty = function
  (* Let expressions *)
  | Let (name, def_ty, def, body) ->
      let def, def_ty =
        match def_ty with
        | None -> infer context def
        | Some def_ty ->
            let def_ty = check context def_ty Semantics.Univ in
            let def_ty' = eval context def_ty in
            let def = check context def def_ty' in
            (Syntax.Ann (def, def_ty), def_ty')
      in
      let context = bind_def context name def_ty (eval context def) in
      let body, body_ty = infer context body in
      (Syntax.Let (name, def, body), body_ty)

  (* Named terms *)
  | Name name ->
      (* Find the index of most recent binding in the context identified by
          [name], starting from the most recent binding. This gives us the
          corresponding de Bruijn index of the variable. *)
      begin match List.elem_index (Some name) context.names with
      | Some index -> (Syntax.Var index, List.nth context.tys index)
      | None -> error (not_bound name)
      end

  (* Annotated terms *)
  | Ann (tm, ty) ->
      let ty = check context ty Semantics.Univ in
      let ty' = eval context ty in
      let tm = check context tm ty' in
      (Syntax.Ann (tm, ty), ty')

    (* Universes *)
  | Univ ->
      (* We use [Type : Type] here for simplicity, which means this type
          theory is inconsistent. This is okay for a toy type system, but we’d
          want look into using universe levels in an actual implementation. *)
      (Syntax.Univ, Semantics.Univ)

  (* Function types *)
  | FunType (params, body_ty) ->
      let rec go context = function
        | [] -> check context body_ty Semantics.Univ
        | (name, param_ty) :: params ->
            let param_ty = check context param_ty Semantics.Univ in
            let context = bind_param context name (eval context param_ty) in
            Syntax.FunType (name, param_ty, go context params)
      in
      (go context params, Semantics.Univ)

  (* Arrow types. These are implemented as syntactic sugar for non-dependent
      function types. *)
  | FunArrow (param_ty, body_ty) ->
      let param_ty = check context param_ty Semantics.Univ in
      let context = bind_param context None (eval context param_ty) in
      let body_ty = check context body_ty Semantics.Univ in
      (Syntax.FunType (None, param_ty, body_ty), Semantics.Univ)

  (* Function literals. These do not have type annotations on their arguments
      and so we don’t know ahead of time what types to use for the arguments
      when adding them to the context. As a result with coose to throw an
      ambiguity error here. *)
  | FunLit (_, _) -> error "ambiguous function literal"

  (* Function application *)
  | RecType decls ->
      let rec go context seen_labels = function
        | [] -> (Syntax.Nil)
        | (label, _) :: _ when List.mem label seen_labels ->
            error ("duplicate label `" ^ label ^ "` in record type")
        | (label, ty) :: decls ->
            let ty = check context ty Semantics.Univ in
            let context = bind_param context (Some label) (eval context ty) in
            Syntax.Cons (label, ty, go context (label :: seen_labels) decls)
      in
      (Syntax.RecType (go context [] decls), Semantics.Univ)

  (* Unit records. These are ambiguous in inference mode. We could default to
      one or the other, and perhaps coerce between them, but we choose just to
      throw an error instead. *)
  | RecLit _ -> error "ambiguous record literal"
  | RecUnit -> error "ambiguous unit record"

  (* Singleton types *)
  | SingType (ty, sing_tm) ->
      let ty = check context ty Semantics.Univ in
      let sing_tm = check context sing_tm (eval context ty) in
      (Syntax.SingType (ty, sing_tm), Semantics.Univ)

  (* Application *)
  | App (head, args) ->
      List.fold_left
        (fun (head, head_ty) arg ->
          match elim_implicits context head head_ty with
          | head, Semantics.FunType (_, param_ty, body_ty) ->
              let arg = check context arg param_ty in
              (Syntax.FunApp (head, arg), body_ty (eval context arg))
          | _ -> error "not a function")
        (infer context head)
        args

  (* Field projection *)
  | Proj (head, labels) ->
      List.fold_left
        (fun (head, head_ty) label ->
          match elim_implicits context head head_ty with
          | head, Semantics.RecType decls ->
              begin match Semantics.proj_ty (eval context head) decls label with
              | Some ty -> (Syntax.RecProj (head, label), ty)
              | None -> error (missing_field label)
              end
          | _ -> error "not a record")
        (infer context head)
        labels

  (* Patches. Here we add patches to record types by creating a copy of the
      type with singletons in place of the patched fields  *)
  | Patch (head, patches) ->
      let rec go context decls patches =
        match decls, patches with
        | Semantics.Nil, [] -> Syntax.Nil
        | Semantics.Nil, (label, _) :: _ ->
            error ("field `" ^ label ^ "` not found in record type")
        | Semantics.Cons (label, ty, tys), patches ->
            let ty' = quote context ty Univ in
            begin match List.assoc_opt label patches with
            | Some patch_tm ->
                let tm = check context patch_tm ty in
                let tm' = eval context tm in
                let context = bind_def context (Some label) (Semantics.SingType (ty, tm')) tm' in
                let patches = List.remove_assoc label patches in
                Syntax.Cons (label, Syntax.SingType (ty', tm), go context (tys tm') patches)
            | None ->
                let var = next_var context in
                let context = bind_def context (Some label) ty var in
                Syntax.Cons (label, ty', go context (tys var) patches)
            end
      in

      let dupes = List.find_dupes (List.map fst patches) in
      if List.compare_length_with dupes 0 <> 0 then
        error ("duplicate labels in patches: `" ^ String.concat "`, `" dupes ^ "`")
      else
        let head = check context head Semantics.Univ in
        begin match eval context head with
        | Semantics.RecType decls ->
            let decls = go context decls patches in
            (Syntax.RecType decls, Semantics.Univ)
        | _ -> error "can only patch record types"
        end

(** {2 Eliminating implicit connectives} *)

(** Connectives that were introduced implicitly during elaboration can
    sometimes get in the way, for example when calling {!coerce}, or when
    elaborating the head of an elimination. This removes them by adding
    appropriate elimination forms. *)
and elim_implicits context tm = function
  (* Convert the singleton back to its underlying term using {!Syntax.SingElim} *)
  | Semantics.SingType (ty, sing_tm) ->
      let tm = Syntax.SingElim (tm, quote context sing_tm ty) in
      elim_implicits context tm ty
  (* TODO: we can eliminate implicit functions here. See the elaboration-zoo
    for ideas on how to do this: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L48-L53 *)
  | ty -> (tm, ty)
