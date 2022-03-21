type index = int
type level = int

type term =
  | Var of index
  | Let of term * term * term
  | UnivType
  (* TODO: | TypeFormat *)
  | TypeFunction of term * term
  | TypeRecord of (string * term) list
  | FunctionLit of term * term
  | FunctionApp of term * term
  | RecordLit of (string * term) list
  | RecordProj of term * string
  (* TODO: | FormatSequence of (string * term) list *)
  (* TODO: | FormatArray of term * term *)
  (* TODO: | FormatRepr of term *)

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
    (* TODO: | TypeFormat *)
    | TypeFunction of value * closure
    | TypeRecord of string list * telescope
    | FunctionLit of value * closure
    | RecordLit of (string * value) list
  and neu =
    | Var of level
    | FunctionApp of neu * value
    | RecordProj of neu * string
    (* TODO: | FormatRepr of neu *)
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

module Pretty = struct

  (* TODO: Precedences *)
  (* TODO: Name binding *)
  let rec term ?(indent = 2) =
    let param param_ty =
      Pp.hvbox ~indent (Pp.concat [
        Pp.hvbox (Pp.concat [
          Pp.text "(";
          Pp.text "_";
          Pp.space;
          Pp.text ":";
          Pp.space;
        ]);
        term param_ty;
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
              term def_ty;
              Pp.space;
              Pp.text "=";
              Pp.space;
            ]);
            term def_expr;
            Pp.text ";";
            Pp.space;
          ]);
          term body_expr;
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
          term body_ty;
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
            term ty;
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
          term body_expr;
        ])
    | FunctionApp (head_expr, arg_expr) ->
        Pp.concat [
          term head_expr;
          Pp.space;
          term arg_expr;
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
          term expr;
        ]))
    | RecordProj (head_expr, label) ->
        Pp.hvbox ~indent (Pp.concat [
          term head_expr;
          Pp.hvbox (Pp.concat [
            Pp.text ".";
            Pp.text label;
          ]);
        ])

end
