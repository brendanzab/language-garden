type ty =
  | Fun of ty * ty
  | Text
  | Bool
  | Int

type tm =
  | Var of string
  | Let of string * ty * tm * tm
  | FunLit of string * ty * tm
  | FunApp of tm * tm
  | TextLit of string
  | TextConcat of tm * tm
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | IntLit of int
  | IntAdd of tm * tm
  (* | NodeLit of (string * tm) list * tm list *)

module Semantics = struct

  (** {2 Semantic domain} *)

  type vtm =
    | Neu of ntm
    | FunLit of string * ty * (vtm -> vtm)
    | TextLit of string
    | BoolLit of bool
    | IntLit of int
    (* | NodeLit of (string * vtm) list * vtm list *)

  and ntm =
    | Var of string
    | FunApp of ntm * vtm
    | TextConcat of ntm * vtm
    | BoolElim of ntm * vtm Lazy.t * vtm Lazy.t
    | IntAdd of ntm * vtm

  type env = (string * vtm) list

  (** {2 Eliminators} *)

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | FunLit (_, _, f) -> f arg
    | Neu nvtm -> Neu (FunApp (nvtm, arg))
    | _ -> invalid_arg "expected function literal"

  let text_concat (vtm1 : vtm) (vtm2 : vtm) : vtm =
    match vtm1, vtm2 with
    | TextLit s1, TextLit s2 -> TextLit (s1 ^ s2)
    | Neu nvtm, _ -> Neu (TextConcat (nvtm, vtm2))
    | _ -> invalid_arg "expected text literal"

  let bool_elim (head : vtm) (vtm1 : vtm Lazy.t) (vtm2 : vtm Lazy.t) : vtm =
    match head with
    | BoolLit true -> Lazy.force vtm1
    | BoolLit false -> Lazy.force vtm2
    | Neu nvtm -> Neu (BoolElim (nvtm, vtm1, vtm2))
    | _ -> invalid_arg "expected boolean literal"

  let int_add (vtm1 : vtm) (vtm2 : vtm) : vtm =
    match vtm1, vtm2 with
    | IntLit i1, IntLit i2 -> IntLit (i1 + i2)
    | Neu nvtm, _ -> Neu (IntAdd (nvtm, vtm2))
    | _ -> invalid_arg "expected int literal"

  (** {2 Evaluation} *)

  let rec eval (locals : env) (tm : tm) : vtm =
    match tm with
    | Var name ->
        List.assoc name locals
    | Let (name, _, def, body) ->
        eval ((name, eval locals def) :: locals) body
    | FunLit (name, ty, body) ->
        FunLit (name, ty, fun v -> eval ((name, v) :: locals) body)
    | FunApp (head, arg) ->
        fun_app (eval locals head) (eval locals arg)
    | TextLit s ->
        TextLit s
    | TextConcat (tm1, tm2) ->
        text_concat (eval locals tm1) (eval locals tm2)
    | BoolLit b ->
        BoolLit b
    | BoolElim (head, tm1, tm2) ->
        bool_elim
          (eval locals head)
          (lazy (eval locals tm1))
          (lazy (eval locals tm2))
    | IntLit n ->
        IntLit n
    | IntAdd (tm1, tm2) ->
        int_add (eval locals tm1) (eval locals tm2)

  (** {2 Quotation} *)

  let rec fresh (ns : string list) (x : string) : string =
    match List.mem x ns with
    | true -> fresh ns (x ^ "'")
    | false -> x

  let rec quote (ns : string list) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu ns ntm
    | FunLit (x, ty, body) ->
        let x = fresh ns x in
        FunLit (x, ty, quote (x :: ns) (body (Neu (Var x))))
    | TextLit s ->
        TextLit s
    | BoolLit b ->
        BoolLit b
    | IntLit n ->
        IntLit n
  and quote_neu (ns : string list) (ntm : ntm) : tm =
    match ntm with
    | Var x -> Var x
    | FunApp (head, arg) ->
        FunApp (quote_neu ns head, quote ns arg)
    | TextConcat (head, vtm) ->
        TextConcat (quote_neu ns head, quote ns vtm)
    | BoolElim (head, vtm1, vtm2) ->
        BoolElim (quote_neu ns head, quote ns (Lazy.force vtm1), quote ns (Lazy.force vtm2))
    | IntAdd (head, vtm) ->
        IntAdd (quote_neu ns head, quote ns vtm)

  (** {2 Normalisation-by-evaluation} *)

  let normalise (vs : env) (e : tm) : tm =
    quote (List.map fst vs) (eval vs e)

end
