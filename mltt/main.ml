(*

  An implementation of MLTT with records

  Inspiration

  - [Typechecking for higher-rank polymorphism](https://gist.github.com/mb64/f49ccb1bbf2349c8026d8ccf29bd158e)
  - [Bidirectional typechecking for higher-rank polymorphism](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7)
  - [Very simple typechecker for MLTT](https://gist.github.com/mb64/4a49d710dcdd1875bebdbc59081acb85)

  TODO:

  - [x] interpreter
  - [x] elaborator
    - [ ] unifier
  - [x] parser
  - [x] pretty printer
  - [ ] REPL
*)

(** Extensions for the {!List} module *)
module List = struct
  include List

  (** Returns the index of the given element in the list *)
  let elem_index a =
    let rec go i = function
      | [] -> None
      | x :: xs -> if x = a then Some i else go (i + 1) xs in
    go 0

end

(** The core language *)
module Core = struct

  type index = int
  type level = int

  type term =
    | Var of index
    | Let of term * term * term
    | UnivType
    | TypeFunction of term * term
    | TypeRecord of (string * term) list
    | FunctionLit of term * term
    | FunctionApp of term * term
    | RecordLit of (string * term) list
    | RecordProj of term * string

  (* TODO: Precedences *)
  (* TODO: Name binding *)
  let rec pp_term ?(indent = 2) =
    let param param_ty =
      Pp.hvbox ~indent (Pp.concat [
        Pp.hvbox (Pp.concat [
          Pp.text "(";
          Pp.text "_";
          Pp.space;
          Pp.text ":";
          Pp.space;
        ]);
        pp_term param_ty;
        Pp.text ")";
      ]) in
    let record fields f =
      match fields with
      | [] -> Pp.text "{}"
      | fields ->
          Pp.concat [
            Pp.text "{";
            Pp.concat_map fields ~f:(fun field ->
              Pp.concat [
                Pp.break ~nspaces:1 ~shift:indent;
                f field;
                Pp.text ";";
              ]);
            Pp.space;
            Pp.text "}";
          ] in

    function
    | Var index -> Pp.text (string_of_int index)
    | Let (def_ty, def_expr, body_expr) ->
        Pp.concat [
          Pp.hvbox ~indent (Pp.concat [
            Pp.hvbox (Pp.concat [
              Pp.hvbox ~indent (Pp.concat [
                Pp.text "let";
                Pp.space;
                Pp.text "_";
                Pp.space;
                Pp.text ":";
                Pp.space;
              ]);
              pp_term def_ty;
              Pp.space;
              Pp.text "=";
              Pp.space;
            ]);
            pp_term def_expr;
            Pp.text ";";
            Pp.space;
          ]);
          pp_term body_expr;
        ]
    | UnivType -> Pp.text "Type"
    | TypeFunction (param_ty, body_ty) ->
        Pp.hvbox ~indent (Pp.concat [
          Pp.hvbox (Pp.concat [
            Pp.text "fun";
            Pp.space;
            Pp.hvbox (param param_ty);
            Pp.space;
            Pp.text "->";
            Pp.space;
          ]);
          pp_term body_ty;
        ])
    | TypeRecord fields ->
        record fields (fun (label, ty) ->
          Pp.hvbox ~indent (Pp.concat [
            Pp.hvbox (Pp.concat [
              Pp.text label;
              Pp.space;
              Pp.text ":";
              Pp.space;
            ]);
            pp_term ty;
          ]))
    | FunctionLit (param_ty, body_expr) ->
        Pp.hvbox ~indent (Pp.concat [
          Pp.hvbox (Pp.concat [
            Pp.text "fun";
            Pp.space;
            Pp.hvbox (param param_ty);
            Pp.space;
            Pp.text "=>";
            Pp.space;
          ]);
          pp_term body_expr;
        ])
    | FunctionApp (head_expr, arg_expr) ->
        Pp.concat [
          pp_term head_expr;
          Pp.space;
          pp_term arg_expr;
        ]
    | RecordLit fields ->
      record fields (fun (label, expr) ->
        Pp.hvbox ~indent (Pp.concat [
          Pp.hvbox (Pp.concat [
            Pp.text label;
            Pp.space;
            Pp.text "=";
            Pp.space;
          ]);
          pp_term expr;
        ]))
    | RecordProj (head_expr, label) ->
        Pp.hvbox ~indent (Pp.concat [
          pp_term head_expr;
          Pp.hvbox (Pp.concat [
            Pp.text ".";
            Pp.text label;
          ]);
        ])

  module Semantics : sig

    type closure
    type telescope

    type value =
      | Neutral of neu
      | UnivType
      (* TODO: | TypeFormat *)
      | TypeFunction of value * closure
      | TypeRecord of string list * telescope
      | FunctionLit of value * closure
      | RecordLit of (string * value) list
    and neu =
      | Var of level
      | FunctionApp of neu * value
      | RecordProj of neu * string

    type env =
      value list

    exception Error of string

    val eval : env -> term -> value
    val quote : level -> value -> term
    val normalise : env -> term -> term

    val closure_app : closure -> value -> value
    val telescope_uncons : telescope -> (value * (value -> telescope)) option

    val function_app : value -> value -> value
    val record_proj : value -> string -> value

    val is_convertible : level -> value -> value -> bool

  end = struct

    type value =
      | Neutral of neu
      | UnivType
      | TypeFunction of value * closure
      | TypeRecord of string list * telescope
      | FunctionLit of value * closure
      | RecordLit of (string * value) list
    and neu =
      | Var of level
      | FunctionApp of neu * value
      | RecordProj of neu * string
    and closure = {
      env : env;
      term : term;
    }
    and telescope =
      | Nil
      | Cons of value * env * term list
    and env = value list

    exception Error of string

    let rec eval env : term -> value = function
      | Var index -> begin
        match List.nth_opt env index with
        | Some value -> value
        | None -> raise (Error "var not found")
      end
      | Let (_, def_expr, body_expr) ->
          eval (eval env def_expr :: env) body_expr
      | UnivType -> UnivType
      | TypeFunction (param_ty, body_ty) ->
          TypeFunction (eval env param_ty, { env; term = body_ty })
      | TypeRecord [] -> TypeRecord ([], Nil)
      | TypeRecord ((label, ty) :: fields) ->
          let labels = List.map (fun (label, _) -> label) fields in
          let tys = List.map (fun (_, ty) -> ty) fields in
          TypeRecord (label :: labels, Cons (eval env ty, env, tys))
      | FunctionLit (param_ty, body) ->
          FunctionLit (eval env param_ty, { env; term = body })
      | FunctionApp (head, arg) ->
          function_app (eval env head) (eval env arg)
      | RecordLit fields ->
          let eval_field (label, expr) = (label, eval env expr) in
          RecordLit (List.map eval_field fields)
      | RecordProj (expr, label) ->
          record_proj (eval env expr) label
    and closure_app closure arg =
      eval (arg :: closure.env) closure.term
    and function_app expr arg =
      match expr with
      | Neutral neu -> Neutral (FunctionApp (neu, arg))
      | FunctionLit (_, body) -> closure_app body arg
      | _ -> raise (Error "invalid record projection")
    and record_proj expr label =
      match expr with
      | Neutral neu -> Neutral (RecordProj (neu, label))
      | RecordLit fields ->
          let rec find_field = function
            | [] -> raise (Error "field not found")
            | ((label', expr) :: _) when label = label' -> expr
            | (_ :: fields) -> find_field fields
          in
          find_field fields
      | _ -> raise (Error "invalid record projection")

    let telescope_uncons = function
      | Nil -> None
      | Cons (value, _, []) ->
          Some (value, fun _ -> Nil)
      | Cons (value, env, term :: terms) ->
          Some (value, fun value -> Cons (eval env term, value :: env, terms))

    let rec quote size : value -> term = function
      | Neutral neu -> quote_neu size neu
      | UnivType -> UnivType
      | TypeFunction (param_ty, body_ty) ->
          TypeFunction (quote size param_ty, quote_closure size body_ty)
      | TypeRecord (labels, tys) ->
          let rec quote_fields size labels tys =
            match labels, telescope_uncons tys with
            | [], None -> []
            | [], _ | _, None -> raise (Error "invalid record type telescope")
            | label :: labels, Some (ty, tys) ->
                let var = Neutral (Var size) in
                (label, quote size ty) :: quote_fields (size + 1) labels (tys var)
          in
          TypeRecord (quote_fields size labels tys)
      | FunctionLit (param_ty, body_expr) ->
          FunctionLit (quote size param_ty, quote_closure size body_expr)
      | RecordLit fields ->
          RecordLit (List.map (fun (label, expr) -> label, quote size expr) fields)
    and quote_neu size : neu -> term = function
      | Var level -> Var (size - level - 1)
      | FunctionApp (neu, arg) -> FunctionApp (quote_neu size neu, quote size arg)
      | RecordProj (neu, label) -> RecordProj (quote_neu size neu, label)
    and quote_closure size closure =
      let var = Neutral (Var size) in
      quote (size + 1) (closure_app closure var)

    let normalise env term =
      let value = eval env term
      in quote (List.length env) value

    let rec is_convertible size value1 value2 =
      match value1, value2 with
      | Neutral nue1, Neutral nue2 ->
          is_convertible_neu size nue1 nue2
      | UnivType, UnivType -> true
      | TypeFunction (param_ty1, body_ty1), TypeFunction (param_ty2, body_ty2) ->
          is_convertible size param_ty1 param_ty2
          && is_convertible_closure size body_ty1 body_ty2
      | TypeRecord (labels1, tys1), TypeRecord (labels2, tys2) ->
          labels1 = labels2
            && is_convertible_tele size tys1 tys2
      | FunctionLit (param_ty1, body_expr1), FunctionLit (param_ty2, body_expr2) ->
          is_convertible size param_ty1 param_ty2
            && is_convertible_closure size body_expr1 body_expr2
      | RecordLit fields1, RecordLit fields2 ->
          let on_field (label1, expr1) (label2, expr2) =
            label1 = label2 && is_convertible size expr1 expr2 in
          List.for_all2 on_field fields1 fields2
      | _, _ -> false
    and is_convertible_neu size nue1 nue2 =
      match nue1, nue2 with
      | Var var1, Var var2 -> var1 = var2
      | FunctionApp (neu1, arg1), FunctionApp (neu2, arg2) ->
          is_convertible_neu size neu1 neu2 && is_convertible size arg1 arg2
      | RecordProj (neu1, label1), RecordProj (neu2, label2) ->
          is_convertible_neu size neu1 neu2 && label1 = label2
      | _, _ -> false
    and is_convertible_closure size closure1 closure2 =
      let var = Neutral (Var size) in
      is_convertible (size + 1) (closure_app closure1 var) (closure_app closure2 var)
    and is_convertible_tele size tele1 tele2 =
      match telescope_uncons tele1, telescope_uncons tele2 with
      | None, None -> true
      | Some _, None | None, Some _ -> false
      | Some (value1, tele1), Some (value2, tele2) ->
          let var = Neutral (Var size) in
          is_convertible size value1 value2
            && is_convertible_tele (size + 1) (tele1 var) (tele2 var)

  end

end

(** The surface language *)
module Surface = struct

  type term =
    | Name of string
    (* TODO: | Placeholder *)
    (* TODO: | Hole of string *)
    | Ann of term * term
    | Let of string option * term * term * term
    | Type
    | Arrow of term * term
    | FunctionType of string option * term * term
    | FunctionLit of string option * term option * term
    | RecordType of (string * term) list
    | RecordLit of (string * term) list
    | Unit
    | App of term * term list
    | Proj of term * string list

  (** Elaboration from the {!Surface} language to the {!Core} language *)
  module Elaboration : sig

    (** Elaboration context *)
    type context

    (** The empty {!context} *)
    val initial_context : context

    (** Bind a new definition in the {!context} *)
    val bind_def : context -> string option -> Core.Semantics.value -> Core.Semantics.value -> context

    (** Bind a new parameter in the {!context} *)
    val bind_param : context -> string option -> Core.Semantics.value -> context


    (** Evaluate a {!Core.term} in the given {!context} *)
    val eval : context -> Core.term -> Core.Semantics.value

    (** Quote a {!Core.Semantics.value} back into a {!Core.term} in the given {!context} *)
    val quote : context -> Core.Semantics.value -> Core.term


    (** Check that a {!term} is a type *)
    val check_ty : context -> term -> (Core.term, string) result

    (** Check that a {!term} is an expression of a given type *)
    val check_term : context -> term -> Core.Semantics.value -> (Core.term, string) result

    (** Synthesize the type of a {!term} *)
    val synth_term : context -> term -> (Core.term * Core.Semantics.value, string) result

  end = struct

    type context = {
      level : Core.level;
      names : (string option) list;
      types : Core.Semantics.value list;
      exprs : Core.Semantics.value list;
    }

    let initial_context = {
      level = 0;
      names = [];
      types = [];
      exprs = [];
    }

    let bind_def context name ty expr = {
      level = context.level + 1;
      names = name :: context.names;
      types = ty :: context.types;
      exprs = expr :: context.exprs;
    }

    let bind_param context name ty = {
      level = context.level + 1;
      names = name :: context.names;
      types = ty :: context.types;
      exprs = Core.Semantics.Neutral (Core.Semantics.Var context.level) :: context.exprs;
    }

    let lookup context name =
      let (let*) = Option.bind in
      let* index = List.elem_index (Some name) context.names in
      let* ty = List.nth_opt context.types index in
      Some (index, ty)

    let eval context =
      Core.Semantics.eval context.exprs
    let quote context =
      Core.Semantics.quote context.level

    let is_convertible context =
      Core.Semantics.is_convertible (List.length context.exprs)

    let (let*) = Result.bind
    (* let (let+) f x = Result.map x f *)

    let rec check_ty context ty =
      check_term context ty Core.Semantics.UnivType

    and check_term context expr expected_ty =
      match expr, expected_ty with
      | Let (def_name, def_ty, def_expr, body_expr), _ ->
          let* def_ty = check_ty context def_ty in
          let def_ty' = eval context def_ty in
          let* def_expr = check_term context def_expr def_ty' in
          let body_context = bind_def context def_name def_ty' (eval context def_expr) in
          let* body_expr = check_term body_context body_expr expected_ty in
          Ok (Core.Let (def_ty, def_expr, body_expr))
      | FunctionLit (param_name, None, body_expr), Core.Semantics.TypeFunction (param_ty, body_ty) ->
          let param_expr = Core.Semantics.Neutral (Core.Semantics.Var context.level) in
          let body_ty = Core.Semantics.closure_app body_ty param_expr in
          let body_context = bind_param context param_name param_ty in
          let* body_expr = check_term body_context body_expr body_ty in
          Ok (Core.FunctionLit (quote context param_ty, body_expr))
      | RecordLit fields, Core.Semantics.TypeRecord (labels, tys) ->
          (* TODO: make tail recursive *)
          let rec check_fields context fields labels tys =
            match fields, labels, Core.Semantics.telescope_uncons tys with
            | [], [], None -> Ok []
            | [], _, _ -> Error "not enough fields in record literal"
            | _, [], _ | _, _, None -> Error "too many fields in record literal"
            | (label, _) :: _, label' :: _, _ when label <> label' ->
                Error ("unexpected field, found `" ^ label ^ "` expected `" ^ label' ^ "`")
            | (label, expr) :: fields, _ :: labels, Some (ty, tys) ->
                let* expr = check_term context expr ty in
                let expr' = eval context expr in
                let fields_context = bind_def context (Some label) ty expr' in
                let* fields = check_fields fields_context fields labels (tys expr') in
                Ok ((label, expr) :: fields)
          in
          let* fields = check_fields context fields labels tys in
          Ok (Core.RecordLit fields)
      | Unit, Core.Semantics.UnivType ->
          Ok (Core.TypeRecord [])
      | Unit, Core.Semantics.TypeRecord ([], _) ->
          Ok (Core.RecordLit [])
      | expr, _ ->
          let* (expr, ty) = synth_term context expr in
          if is_convertible context ty expected_ty then
            Ok expr
          else
            Error "mismatched types"

    and synth_term context expr =
      match expr with
      | Name name -> begin
        match lookup context name with
        | Some (index, ty) -> Ok (Core.Var index, ty)
        | None -> Error ("name `" ^ name ^ "` not in scope")
      end
      | Ann (expr, ty) ->
          let* ty = check_ty context ty in
          let ty' = eval context ty in
          let* expr = check_term context expr ty' in
          Ok (expr, ty')
      | Let (def_name, def_ty, def_expr, body_expr) ->
          let* def_ty = check_ty context def_ty in
          let def_ty' = eval context def_ty in
          let* def_expr = check_term context def_expr def_ty' in
          let body_context = bind_def context def_name def_ty' (eval context def_expr) in
          let* (body_expr, body_ty) = synth_term body_context body_expr in
          Ok (Core.Let (def_ty, def_expr, body_expr), body_ty)
      | Type ->
          Ok (Core.UnivType, Core.Semantics.UnivType)
      | Arrow (param_ty, body_ty) ->
          let* param_ty = check_ty context param_ty in
          let body_context = bind_param context None (eval context param_ty) in
          let* body_ty = check_ty body_context body_ty in
          Ok (Core.TypeFunction (param_ty, body_ty), Core.Semantics.UnivType)
      | FunctionType (param_name, param_ty, body_ty) ->
          let* param_ty = check_ty context param_ty in
          let body_context = bind_param context param_name (eval context param_ty) in
          let* body_ty = check_ty body_context body_ty in
          Ok (Core.TypeFunction (param_ty, body_ty), Core.Semantics.UnivType)
      | FunctionLit (_, _, _) ->
          Error "type annotations required"
      | RecordType fields ->
          (* TODO: make tail recursive *)
          let rec check_fields context seen_labels = function
            | [] -> Ok []
            | (label, _) :: _ when List.mem label seen_labels ->
                Error ("duplicate label `" ^ label ^ "` encountered in record type")
            | (label, ty) :: fields ->
                let* ty = check_ty context ty in
                let fields_context = bind_param context (Some label) (eval context ty) in
                let* fields = check_fields fields_context (label :: seen_labels) fields in
                Ok ((label, ty) :: fields)
          in
          let* fields = check_fields context [] fields in
          Ok (Core.TypeRecord fields, Core.Semantics.UnivType)
      | RecordLit _ ->
          Error "type annotations required for record literal"
      | Unit ->
          Error "type annotations required for unit literal"
      | App (head_expr, arg_exprs) ->
          List.fold_left
            (fun head_expr arg_expr ->
              let* (head_expr, head_ty) = head_expr in
              match head_ty with
              | Core.Semantics.TypeFunction (param_ty, body_ty) ->
                  let* arg_expr = check_term context arg_expr param_ty in
                  let body_ty = Core.Semantics.closure_app body_ty (eval context arg_expr) in
                  Ok (Core.FunctionApp (head_expr, arg_expr), body_ty)
              | _ -> Error "function expected")
            (synth_term context head_expr)
            arg_exprs
      | Proj (head_expr, labels) ->
          List.fold_left
            (fun head_expr label ->
              let* (head_expr, head_ty) = head_expr in
              match head_ty with
              | Core.Semantics.TypeRecord (labels, tys) ->
                  let head_expr' = eval context head_expr in
                  let rec get_ty labels tys =
                    match labels, Core.Semantics.telescope_uncons tys with
                    | [], _ | _, None -> Error ("label `" ^ label ^ "` not found in record")
                    | label' :: _, Some (ty, _) when label = label' -> Ok ty
                    | _ :: labels, Some (_, tys) ->
                        get_ty labels (tys (Core.Semantics.record_proj head_expr' label))
                  in
                  let* ty = get_ty labels tys in
                  Ok (Core.RecordProj (head_expr, label), ty)
              | _ -> Error "record expected")
            (synth_term context head_expr)
            labels

  end

  (** Distillation from the {!Core} language to the {!Surface} language *)
  module Distill : sig
    type context

    val context_with_names : string option -> context

  end = struct

    type context = {
      names : string list;
      fresh : int;
    }

    let context_with_names _names =
      failwith "todo"

  end

  (** Surface language parser *)
  module Parse : sig

    val from_string : string -> (term, string) result

  end = struct

    (* https://github.com/inhabitedtype/angstrom/blob/master/lib/angstrom.mli *)
    (* https://gist.github.com/mb64/f49ccb1bbf2349c8026d8ccf29bd158e#file-tychk_nbe-ml-L187-L255 *)

    open Angstrom

    (** String and character predicates *)
    module Pred = struct

      let is_keyword = function
        | "Type" | "let" | "in" | "fun" -> true
        | _ -> false

      let is_whitespace = function
        | ' ' | '\n' | '\t' -> true
        | _ -> false

      let is_ident_char = function
        | '-' | 'a'..'z' | 'A'..'Z' -> true
        | _ -> false

    end

    let whitespace =
      skip_while Pred.is_whitespace

    let lexeme parser =
      parser <* whitespace

    let token expected =
      lexeme (string expected) *> return ()

    let keyword_or_ident =
      lexeme (take_while1 Pred.is_ident_char <?> "identifier or keyword")

    let ident =
      let* source = keyword_or_ident in
      if Pred.is_keyword source then fail ("`" ^ source ^ "` is a keyword") else return source

    let keyword expected =
      let* source = keyword_or_ident in
      if source = expected then return () else fail ("expected `" ^ expected ^ "`")

    let parens parser = token "(" *> parser <* token ")"
    let braces parser = token "{" *> parser <* token "}"

    let trailing_sep_by1 sep parser =
      sep_by1 sep parser <* option () sep

    let name_or_placeholder =
      choice [
        token "_" *> return None;
        ident >>| Option.some;
      ]

    let term = fix (fun term ->
      let atomic_term =
        let type_term = token "Type" *> return Type in
        let name_term = ident >>| fun name -> Name name in
        let parens_term = parens term in
        let unit_term = token "{" *> token "}" *> return Unit in

        let record_ty =
          let field =
            let+ label = ident
            and+ () = token ":"
            and+ ty = term in
            (label, ty) in

          let+ fields = braces (trailing_sep_by1 (token ";") field) in
          RecordType fields in

        let record_lit =
          let field =
            let+ label = ident
            and+ () = token "="
            and+ expr = term in
            (label, expr) in

          let+ fields = braces (trailing_sep_by1 (token ";") field) in
          RecordLit fields in

        choice [
          type_term;
          name_term;
          parens_term;
          unit_term;
          record_ty;
          record_lit;
        ] in

      let proj_expr =
        let+ head_expr = atomic_term
        and+ labels = many (token "." *> ident) in
        match labels with
        | [] -> head_expr
        | labels -> Proj (head_expr, labels) in

      let app_expr =
        let+ head_expr = proj_expr
        and+ arg_exprs = many proj_expr in
        match arg_exprs with
        | [] -> head_expr
        | arg_exprs -> App (head_expr, arg_exprs) in

      (* let rec arrow_ty =
        let+ param_ty = app_expr
        and+ () = token "->"
        and+ body_ty = arrow_ty in
        Arrow (param_ty, body_ty) in *)

      (* FIXME: the rest of this is a bit messy... *)

      let param =
        let+ () = token "("
        and+ name = name_or_placeholder
        and+ () = token ":"
        and+ ty = term
        and+ () = token ")" in
        (name, ty) in

      let function_ty =
        let+ () = token "fun"
        and+ (param_name, param_ty) = param
        and+ () = token "->"
        and+ body_expr = term in
        FunctionType (param_name, param_ty, body_expr) in

      let function_or_app =
        choice [
          function_ty;
          app_expr;
        ] in

      let function_lit =
        let+ () = token "fun"
        and+ (param_name, param_ty) = choice [
          name_or_placeholder >>| (fun name -> name, None);
          param >>| (fun (name, ty) -> name, Some ty);
        ]
        and+ () = token "=>"
        and+ body_expr = term in
        FunctionLit (param_name, param_ty, body_expr) in

      let let_expr =
        let+ () = keyword "let"
        and+ def_name = name_or_placeholder
        and+ () = token ":"
        and+ def_ty = function_or_app
        and+ () = token "="
        and+ def_expr = term
        and+ () = token ";"
        and+ body_expr = term in
        Let (def_name, def_ty, def_expr, body_expr) in

      let+ expr =
        choice [
          let_expr;
          function_ty;
          function_lit;
          app_expr;
        ]
      and+ tys = many (token ":" *> function_or_app) in
      List.fold_left (fun expr ty -> Ann (expr, ty)) expr tys
    )

    let from_string =
      parse_string ~consume:All (whitespace *> term)

  end

end

let main () =
  let stdin = Stdio.In_channel.(input_all stdin) in

  let term =
    match Surface.Parse.from_string stdin with
    | Ok term -> term
    | Error message ->
        print_endline ("error: " ^ message);
        exit 1
  in

  Surface.Elaboration.(
    let context = initial_context in
    match synth_term context term with
    | Ok (expr, ty) ->
        let ty = quote context ty in
        Format.printf "%a@." Pp.to_fmt
          (Pp.hvbox ~indent:2 (Pp.concat [
            Pp.hvbox ~indent:2 (Pp.concat [
              Pp.hvbox ~indent:2 (Pp.concat [
                Pp.text "input";
                Pp.space;
                Pp.text ":";
              ]);
              Pp.space;
              Pp.hvbox (Core.pp_term ty);
              Pp.space;
              Pp.text "=";
            ]);
            Pp.space;
            Pp.hvbox (Core.pp_term expr);
          ]));
        exit 0
    | Error message ->
        print_endline ("error: " ^ message);
        exit 1
  )

let () = main ()
