module Syntax = CoreSyntax

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
  | Syntax.Var index -> Pp.text ("@" ^ string_of_int index)
  | Syntax.Let (def_ty, def_expr, body_expr) ->
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
  | Syntax.UnivType -> Pp.text "Type"
  | Syntax.TypeFunction (param_ty, body_ty) ->
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
  | Syntax.TypeRecord fields ->
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
  | Syntax.FunctionLit (param_ty, body_expr) ->
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
  | Syntax.FunctionApp (head_expr, arg_expr) ->
      Pp.concat [
        term head_expr;
        Pp.space;
        term arg_expr;
      ]
  | Syntax.RecordLit fields ->
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
  | Syntax.RecordProj (head_expr, label) ->
      Pp.hvbox ~indent (Pp.concat [
        term head_expr;
        Pp.hvbox (Pp.concat [
          Pp.text ".";
          Pp.text label;
        ]);
      ])
