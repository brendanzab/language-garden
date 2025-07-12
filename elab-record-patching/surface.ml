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

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

type label = string spanned
type pattern = string option spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Let of pattern * params * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                                  (** References to named things: [ x ] *)
  | Ann of tm * tm                                  (** Terms annotated with types: [ x : A ] *)
  | Univ                                            (** Universe of types: [ Type ] *)
  | Fun_type of params * tm                         (** Function types: [ fun (x : A) -> B x ] *)
  | Fun_arrow of tm * tm                            (** Function arrow types: [ A -> B ] *)
  | Fun_lit of params * tm option * tm              (** Function literals: [ fun x => f x ] *)
  | Rec_type of (label * tm) list                   (** Record types: [ { x : A; ... } ]*)
  | Rec_lit of (label * (params * tm) option) list  (** Record literals: [ { x := A; ... } ]*)
  | Rec_unit                                        (** Unit records: [ {} ] *)
  | Sing_type of tm * tm                            (** Singleton types: [ A [= x ] ] *)
  | App of tm * tm list                             (** Applications: [ f x ] *)
  | Proj of tm * label list                         (** Projections: [ r.l ] *)
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


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check : tm -> Core.Semantics.vty -> (Core.Syntax.tm, span * string) result
  val infer : tm -> (Core.Syntax.tm * Core.Semantics.vty, span * string) result

end = struct

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics


  (** {2 Elaboration state} *)

  (** The elaboration context records the bindings that are currently bound at
      the current scope in the program. The environments are unzipped to make it
      more efficient to call functions from {!Core.Semantics}. *)
  type context = {
    size : Core.level;                      (** Number of entries bound. *)
    names : Core.name Core.env;             (** Name environment *)
    ty_env : Semantics.vty Core.env;        (** Type environment *)
    tm_env : Semantics.vtm Lazy.t Core.env; (** Term environment *)
  }

  (** The empty context *)
  let empty = {
    size = 0;
    names = [];
    ty_env = [];
    tm_env = [];
  }

  (** Returns the next variable that will be bound in the context after calling
      {!bind_def} or {!bind_param} *)
  let next_var (ctx : context) : Semantics.vtm Lazy.t =
    Lazy.from_val (Semantics.Neu (Semantics.Var ctx.size))

  (** Binds a definition in the context *)
  let bind_def (ctx : context) (name : string option) (vty : Semantics.vty) (vtm : Semantics.vtm Lazy.t) = {
    size = ctx.size + 1;
    names = name :: ctx.names;
    ty_env = vty :: ctx.ty_env;
    tm_env = vtm :: ctx.tm_env;
  }

  (** Binds a parameter in the context *)
  let bind_param (ctx : context) (name : string option) (vty : Semantics.vty) =
    bind_def ctx name vty (next_var ctx)

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Semantics.vty) option =
    (* Find the index of most recent binding in the context identified by
        [name], starting from the most recent binding. This gives us the
        corresponding de Bruijn index of the variable. *)
    ctx.names |> List.find_mapi @@ fun index name' ->
      match Some name = name' with
      | true -> Some (index, List.nth ctx.ty_env index)
      | false -> None

  (** {3 Functions related to the core semantics} *)

  (** These wrapper functions make it easier to call functions from the
      {!Core.Semantics} using state from the elaboration context. *)

  let eval ctx : Syntax.tm -> Semantics.vtm =
    Semantics.eval ctx.tm_env

  let quote ctx : Semantics.vtm -> Syntax.tm =
    Semantics.quote ctx.size

  let is_convertible ctx : Semantics.vtm -> Semantics.vtm -> bool =
    Semantics.is_convertible ctx.size

  let pp ?(resugar = true) ctx : Syntax.tm -> Format.formatter -> unit =
    Syntax.pp ~resugar ctx.names


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of span * string

  (** Raises an {!Error} exception *)
  let error (type a) (span : span) (message : string) : a =
    raise (Error (span, message))

  let type_mismatch (ctx : context) ~expected ~found : string =
    Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %t@]@ @[found:    %t@]@]"
      (pp ctx expected)
      (pp ctx found)

  let singleton_mismatch (ctx : context) ~expected ~found ~ty : string =
    Format.asprintf "@[<v 2>@[singleton mismatch@]@ @[expected: %t@]@ @[found:    %t@]@ @[type:     %t@]@]"
      (pp ctx expected)
      (pp ctx found)
      (pp ctx ty)

  let field_mismatch ~expected ~found : string =
    Format.asprintf "@[<v 2>@[field mismatch@]@ @[expected label: `%s`@]@ @[found label:    `%s`@]@]"
      expected
      found

  let missing_field (label : string) : string =
    Format.asprintf "field with label `%s` not found in record" label

  let not_bound (name : string) : string =
    Format.asprintf "`%s` is not bound in the current scope" name

  let ambiguous_param (name : string option) : string =
      Format.asprintf "ambiguous function parameter `%s`"
        (Option.value ~default:"_" name)


  (** {2 Coercive subtyping} *)

  (** Returns a coercion from a term of one type to a term of another type. By
      performing coercions during elaboration we avoid having to introduce
      subtyping in the core language. *)
  let rec coerce (span : span) (ctx : context) (from_vty : Semantics.vty) (to_vty : Semantics.vty) (tm : Syntax.tm) : Syntax.tm =
    (* TODO: Return [tm] unchanged if no coercion was needed, avoiding unnecessary
      eta-expansions to the elaborated terms. An example of this can be seen here:
      https://github.com/AndrasKovacs/staged/blob/9e381eb162f44912d70fb843c4ca6567b0d1683a/demo/Elaboration.hs#L87-L140 *)

    match from_vty, to_vty with
    (* No need to coerce the term if both types are already the same! *)
    | from_vty, to_vty when is_convertible ctx from_vty to_vty -> tm
    (* Coerce the term to a singleton with {!Syntax.Sing_intro}, if the term is
      convertible to the term expected by the singleton *)
    | from_vty, Semantics.Sing_type (to_vty, sing_tm) ->
        let tm = coerce span ctx from_vty to_vty tm in
        let vtm = eval ctx tm in
        if is_convertible ctx (Lazy.force sing_tm) vtm then Syntax.Sing_intro else
          error span (singleton_mismatch ctx
            ~expected:(quote ctx (Lazy.force sing_tm))
            ~found:(quote ctx vtm)
            ~ty:(quote ctx to_vty))
    (* Coerce the singleton back to its underlying term with {!Syntax.Sing_elim}
      and attempt further coercions from its underlying type *)
    | Semantics.Sing_type (from_vty, sing_tm), to_vty ->
        coerce span ctx from_vty to_vty (quote ctx (Lazy.force sing_tm))
    (* Coerce the fields of a record with record eta expansion *)
    | Semantics.Rec_type from_decls, Semantics.Rec_type to_decls ->
        (* TODO: bind [tm] to a local variable to avoid duplicating records *)
        let rec go from_decls to_decls =
          match from_decls, to_decls with
          | Semantics.Nil, Semantics.Nil -> []
          (* Use eta-expansion to coerce fields that share the same label *)
          | Semantics.Cons (from_label, from_vty, from_decls)
          , Semantics.Cons (to_label, to_vty, to_decls) when from_label = to_label ->
              let from_vtm = lazy (eval ctx (Syntax.Rec_proj (tm, from_label))) in
              let to_tm = coerce span ctx from_vty to_vty (Syntax.Rec_proj (tm, from_label)) in
              (to_label, to_tm) :: go (from_decls from_vtm) (to_decls (lazy (eval ctx to_tm)))
          (* When the type of the target field is a singleton we can use it to
              fill in the definition of a missing field in the source term. This
              is similar to how we handle missing fields in {!check}. *)
          | from_decls, Semantics.Cons (to_label, Semantics.Sing_type (_, _), to_decls) ->
              let to_tm = Syntax.Sing_intro in
              (to_label, to_tm) :: go from_decls (to_decls (lazy (eval ctx to_tm)))
          | Semantics.Cons (from_label, _, _), Semantics.Cons (to_label, _, _) ->
              error span (field_mismatch ~expected:to_label ~found:from_label)
          | _, _ -> Semantics.error "mismatched telescope length"
        in
        Syntax.Rec_lit (go from_decls to_decls)
    (* TODO: subtyping for functions! *)
    | from_vty, to_vty  ->
        error span (type_mismatch ctx
          ~expected:(quote ctx to_vty)
          ~found:(quote ctx from_vty))


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors, and provide enough {i control} to the
      algorithm to keep type inference decidable even in the presence of ‘fancy’
      types, for example dependent types, higher rank types, and subtyping. *)

  (** Elaborate a term in the surface language into a term in the core language
      in the presence of a type annotation. *)
  let rec check (ctx : context) (tm : tm) (vty : Semantics.vty) : Syntax.tm =
    match tm.data, vty with
    (* Let expressions *)
    | Let (name, params, def_ty, def, body), vty ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let def_vty = eval ctx def_ty in
        let body = check (bind_def ctx name.data def_vty (lazy (eval ctx def))) body vty in
        Syntax.Let (name.data, def_ty, def, body)

    (* Function literals *)
    | Fun_lit (params, body_ty, body), vty ->
        check_fun_lit ctx params body_ty body vty

    (* Record literals *)
    | Rec_lit defns, Semantics.Rec_type decls ->
        (* TODO: elaborate fields out of order? *)
        let rec go defns decls =
          match defns, decls with
          | [], Semantics.Nil -> []
          (* When the labels match, check the term against the type, handling
            punned fields appropriately. *)
          | (label, tm) :: defns, Semantics.Cons (label', ty, decls) when label.data = label' ->
              let tm = match tm with
                | Some (params, tm) -> check_fun_lit ctx params None tm ty (* explicit field definition *)
                | None -> check ctx ({ span = label.span; data = Name label.data }) ty (* punned field definition *)
              in
              (label.data, tm) :: go defns (decls (lazy (eval ctx tm)))
          (* When the expected type of a field is a singleton we can use it to
              fill in the definition of a missing fields in the record literal. *)
          | defns, Semantics.Cons (label, Semantics.Sing_type (_, _), decls) ->
              let tm = Syntax.Sing_intro in
              (label, tm) :: go defns (decls (lazy (eval ctx tm)))
          | _, Semantics.Cons (label, _, _) -> error tm.span (missing_field label)
          | (label, _) :: _, Semantics.Nil -> error label.span ("unexpected field `" ^ label.data ^ "` in record literal")
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
    | _, Semantics.Sing_type (vty, sing_vtm) ->
        let tm_span = tm.span in
        let tm = check ctx tm vty in
        let vtm = eval ctx tm in
        if is_convertible ctx (Lazy.force sing_vtm) vtm then Syntax.Sing_intro else
          error tm_span (singleton_mismatch ctx
            ~expected:(quote ctx (Lazy.force sing_vtm))
            ~found:(quote ctx vtm)
            ~ty:(quote ctx vty))

    (* For anything else, try inferring the type of the term, then attempting to
        coerce the term to the expected type. *)
    | _, vty ->
        let tm_span = tm.span in
        let tm, found_vty = infer ctx tm in
        let tm, found_vty = elim_implicits ctx tm found_vty in
        coerce tm_span ctx found_vty vty tm

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer (ctx : context) (tm : tm) : Syntax.tm * Semantics.vty =
    match tm.data with
    (* Let expressions *)
    | Let (name, params, def_ty, def, body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let def_vty = eval ctx def_ty in
        let body, body_ty = infer (bind_def ctx name.data def_vty (lazy (eval ctx def))) body in
        Syntax.Let (name.data, def_ty, def, body), body_ty

    (* Named terms *)
    | Name name ->
        begin match lookup ctx name with
        | Some (index, vty) -> (Syntax.Var index, vty)
        | None -> error tm.span (not_bound name)
        end

    (* Annotated terms *)
    | Ann (tm, ty) ->
        let ty = check ctx ty Semantics.Univ in
        let vty = eval ctx ty in
        check ctx tm vty, vty

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
          | (name, None) :: _ -> error name.span (ambiguous_param name.data)
          | (name, Some param_ty) :: params ->
              let param_ty = check ctx param_ty Semantics.Univ in
              let body_ty = go (bind_param ctx name.data (eval ctx param_ty)) params in
              Syntax.Fun_type (name.data, param_ty, body_ty)
        in
        go ctx params, Semantics.Univ

    (* Arrow types. These are implemented as syntactic sugar for non-dependent
        function types. *)
    | Fun_arrow (param_ty, body_ty) ->
        let param_ty = check ctx param_ty Semantics.Univ in
        let body_ty = check (bind_param ctx None (eval ctx param_ty)) body_ty Semantics.Univ in
        Syntax.Fun_type (None, param_ty, body_ty), Semantics.Univ

    (* Function literals. *)
    | Fun_lit (params, body_ty, body) ->
        let fun_tm, fun_ty = infer_fun_lit ctx params body_ty body in
        fun_tm, eval ctx fun_ty

    (* Function application *)
    | Rec_type decls ->
        let rec go ctx seen_labels = function
          | [] -> []
          | (label, _) :: _ when List.mem label.data seen_labels ->
              error label.span ("duplicate label `" ^ label.data ^ "` in record type")
          | (label, ty) :: decls ->
              let ty = check ctx ty Semantics.Univ in
              let ctx = bind_param ctx (Some label.data) (eval ctx ty) in
              (label.data, ty) :: go ctx (label.data :: seen_labels) decls
        in
        Syntax.Rec_type (go ctx [] decls), Semantics.Univ

    (* Unit records. These are ambiguous in inference mode. We could default to
        one or the other, and perhaps coerce between them, but we choose to throw
        an ambiguity error instead. *)
    | Rec_lit _ -> error tm.span "ambiguous record literal"
    | Rec_unit -> error tm.span "ambiguous unit record"

    (* Singleton types *)
    | Sing_type (ty, sing_tm) ->
        let ty = check ctx ty Semantics.Univ in
        let sing_tm = check ctx sing_tm (eval ctx ty) in
        Syntax.Sing_type (ty, sing_tm), Semantics.Univ

    (* Application *)
    | App (head, args) ->
        List.fold_left
          (fun (head, head_vty) arg ->
            match head_vty with
            | Semantics.Fun_type (_, param_vty, body_vty) ->
                let arg = check ctx arg (Lazy.force param_vty) in
                Syntax.Fun_app (head, arg), body_vty (lazy (eval ctx arg))
            | _ -> error arg.span "unexpected argument")
          (infer ctx head)
          args

    (* Field projection *)
    | Proj (head, labels) ->
        List.fold_left
          (fun (head, head_vty) label ->
            match elim_implicits ctx head head_vty with
            | head, Semantics.Rec_type decls ->
                begin match Semantics.proj_ty (eval ctx head) decls label.data with
                | Some vty -> Syntax.Rec_proj (head, label.data), vty
                | None -> error label.span (missing_field label.data)
                end
            | _ -> error label.span (missing_field label.data))
          (infer ctx head)
          labels

    (* Patches. Here we add patches to record types by creating a copy of the
        type with singletons in place of the patched fields  *)
    | Patch ({ span = head_span; _ } as head, patches) ->
        let rec go ctx decls patches =
          match decls, patches with
          | Semantics.Nil, [] -> []
          | Semantics.Nil, (label, _) :: _ ->
              (* FIXME: use label location *)
              error head.span ("field `" ^ label ^ "` not found in record type")
          | Semantics.Cons (label, vty, ty_env), patches ->
              let ty = quote ctx vty in
              begin match List.assoc_opt label patches with
              | Some patch_tm ->
                  let tm = check ctx patch_tm vty in
                  let vtm = lazy (eval ctx tm) in
                  let ctx = bind_def ctx (Some label) (Semantics.Sing_type (vty, vtm)) vtm in
                  let patches = List.remove_assoc label patches in
                  (label, Syntax.Sing_type (ty, tm)) :: go ctx (ty_env vtm) patches
              | None ->
                  let var = next_var ctx in
                  let ctx = bind_def ctx (Some label) vty var in
                  (label, ty) :: go ctx (ty_env var) patches
              end
        in

        let dupes = find_dupes (List.map fst patches) in
        if List.compare_length_with dupes 0 <> 0 then
          error head.span ("duplicate labels in patches: `" ^ String.concat "`, `" dupes ^ "`")
        else
          let head = check ctx head Semantics.Univ in
          begin match eval ctx head with
          | Semantics.Rec_type decls ->
              let decls = go ctx decls patches in
              Syntax.Rec_type decls, Semantics.Univ
          | _ -> error head_span "can only patch record types"
          end

  (** Elaborate a function literal in checking mode. *)
  and check_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) (vty : Semantics.vty) =
    match params, body_ty, vty with
    | [], None, vty -> check ctx body vty
    | [], Some ({ span = body_ty_span; _ } as body_ty), vty ->
        let body_ty = check ctx body_ty Semantics.Univ in
        let body_vty = eval ctx body_ty in
        if is_convertible ctx body_vty vty then
          check ctx body body_vty
        else error body_ty_span (type_mismatch ctx
          ~expected:(quote ctx vty)
          ~found:body_ty)
    | (name, param_ty) :: params, body_ty, Semantics.Fun_type (_, param_vty', body_vty') ->
        let var = next_var ctx in
        let param_ty =
          match param_ty with
          | None -> Lazy.force param_vty'
          | Some param_ty ->
              let param_ty = check ctx param_ty Semantics.Univ in
              let param_vty = eval ctx param_ty in
              (* Check that the parameter annotation in the function literal
                  matches the expected parameter type. *)
              if is_convertible ctx param_vty (Lazy.force param_vty') then param_vty else
                error name.span (type_mismatch ctx
                  ~expected:(quote ctx (Lazy.force param_vty'))
                  ~found:param_ty)
        in
        let ctx = bind_def ctx name.data param_ty var in
        let body = check_fun_lit ctx params body_ty body (body_vty' var) in
        Syntax.Fun_lit (name.data, body)
    | (name, _) :: _, _, _ ->
        error name.span "too many parameters in function literal"

  (** Elaborate a function literal in inference mode. *)
  and infer_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) =
    match params, body_ty with
    | [], None ->
        let body, body_vty = infer ctx body in
        body, quote ctx body_vty
    | [], Some body_ty ->
        let body_ty = check ctx body_ty Semantics.Univ in
        check ctx body (eval ctx body_ty), body_ty
    | (name, param_ty) :: params, body_ty ->
        let var = next_var ctx in
        let param_ty =
          match param_ty with
          (* We’re in inference mode, so function parameters need annotations *)
          | None -> error name.span (ambiguous_param name.data)
          | Some param_ty -> check ctx param_ty Semantics.Univ
        in
        let ctx = bind_def ctx name.data (eval ctx param_ty) var in
        let body, body_ty = infer_fun_lit ctx params body_ty body in
        Syntax.Fun_lit (name.data, body), Syntax.Fun_type (name.data, param_ty, body_ty)


  (** {2 Eliminating implicit connectives} *)

  (** Connectives that were introduced implicitly during elaboration can
      sometimes get in the way, for example when calling {!coerce}, or when
      elaborating the head of an elimination. This removes them by applying
      appropriate elimination forms. *)
  and elim_implicits (ctx : context) (tm : Syntax.tm) (vty : Semantics.vty) : Syntax.tm * Semantics.vty =
    match vty with
    (* Eliminate the singleton, converting it back to its underlying term *)
    | Sing_type (vty, sing_vtm) ->
        elim_implicits ctx (quote ctx (Lazy.force sing_vtm)) vty
    (* TODO: we can eliminate implicit functions here. See the elaboration-zoo
      for ideas on how to do this: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L48-L53 *)
    | vty -> tm, vty


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : unit -> a) : (a, span * string) result =
    match prog () with
    | result -> Ok result
    | exception Error (span, message) -> Error (span, message)


  (** {2 Public API} *)

  let check (tm : tm) (vty : Semantics.vty) : (Core.Syntax.tm, span * string) result =
    run_elab (fun () -> check empty tm vty)

  let infer (tm : tm) : (Core.Syntax.tm * Core.Semantics.vty, span * string) result =
    run_elab (fun () -> infer empty tm)

end
