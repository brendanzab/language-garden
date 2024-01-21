[@@@warning "-duplicate-definitions"]

type tm =
  | Name of string
  | Let of string * tm * tm
  | Add of tm * tm
  | Template of template
  | IfThenElse of tm * tm * tm
  | App of tm * tm
  | TextLit of string
  | IntLit of int

and template =
  fragment list

and fragment =
  | Text of string
  | Term of tm
  | Let of string * tm
  | IfThenElse of tm * template * template

type context =
  (string * Core.ty) list

let rec elab_check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  match tm with
  | IfThenElse (tm1, tm2, tm3) ->
      BoolElim (
        elab_check context tm1 Bool,
        elab_check context tm2 ty,
        elab_check context tm3 ty)
  | tm ->
      let tm, ty' = elab_synth context tm in
      if ty = ty' then tm else
        failwith "type mismatch"

and elab_synth (context : context) (tm : tm) : Core.tm * Core.ty =
  match tm with
  | Name name -> Var name, List.assoc name context
  | Let (name, def_tm, body_tm) ->
      let def_tm, def_ty = elab_synth context def_tm in
      let body_tm, body_ty = elab_synth ((name, def_ty) :: context) body_tm in
      Let (name, def_ty, def_tm, body_tm), body_ty
  | Template template -> elab_template context template, Text
  | Add (tm1, tm2) ->
      TextConcat (elab_check context tm1 Text, elab_check context tm2 Text), Text
  | IfThenElse (_, _, _) ->
      failwith "ambiguous if expression"
  | App (head_tm, arg_tm) -> begin
      match elab_synth context head_tm with
      | head_tm, Fun (param_ty, body_ty) ->
          let arg_tm = elab_check context arg_tm param_ty in
          FunApp (head_tm, arg_tm), body_ty
      | _ ->
          failwith "function expected"
  end
  | TextLit s -> TextLit s, Text
  | IntLit n -> IntLit n, Int

and[@tail_mod_cons] elab_template (context : context) (template : template) : Core.tm =
  match template with
  | [] -> TextLit ""
  | Text s :: template ->
      TextConcat (TextLit s, (elab_template [@tailcall]) context template)
  | Term tm :: template ->
      TextConcat (elab_check context tm Text, (elab_template [@tailcall]) context template)
  | Let (name, def_tm) :: template ->
      let def_tm, def_ty = elab_synth context def_tm in
      Let (name, def_ty, def_tm, (elab_template [@tailcall]) ((name, def_ty) :: context) template)
  | IfThenElse (tm, template1, template2) :: template ->
      TextConcat (
        BoolElim (
          elab_check context tm Bool,
          elab_template context template1,
          elab_template context template2),
        (elab_template [@tailcall]) context template)
