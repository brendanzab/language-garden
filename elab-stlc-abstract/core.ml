open De_bruijn

(* Syntax *)

type name = string

type ty =
  | Fun_type of ty * ty
  | Int_type
  | Bool_type

type tm =
  | Var of Index.t
  | Ann of tm * ty
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm


(* Pretty printing *)

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty ppf =
    match ty with
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty ppf
  and pp_atomic_ty ty ppf =
    match ty with
    | Bool_type -> Format.fprintf ppf "Bool"
    | Int_type -> Format.fprintf ppf "Int"
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
  in
  pp_ty

let pp_name_ann (name : string) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%s :@]@ %t@]" name (pp_ty ty)

let pp_param (name : string) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %t)@]" name (pp_ty ty)

let pp_tm : tm -> Format.formatter -> unit =
  let rec pp_tm names tm ppf =
    match tm with
    | Let _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (Env.extend name names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names tm)
    | Fun_lit _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Fun_lit (name, param_ty, (Fun_lit _ as body)) ->
              Format.fprintf ppf "@[fun@ %t@ =>@]@ %t"
                (pp_param name param_ty)
                (go (Env.extend name names) body)
          | Fun_lit (name, param_ty, body) ->
              Format.fprintf ppf "@[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (Env.extend name names) body)
          | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<hv 2>@[<hv>%t" (go names tm)
    | Bool_elim (head, tm1, tm2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm names head)
          (pp_app_tm names tm1)
          (pp_tm names tm2)
    | tm ->
        pp_app_tm names tm ppf
  and pp_app_tm names tm ppf =
    match tm with
    | Fun_app (head, arg) ->
        Format.fprintf ppf "@[%t@ %t@]"
          (pp_app_tm names head)
          (pp_atomic_tm names arg)
    | tm ->
        pp_atomic_tm names tm ppf
  and pp_atomic_tm names tm ppf =
    match tm with
    | Var index -> Format.fprintf ppf "%s" (Env.lookup index names)
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Ann _ | Let _ | Fun_lit _ | Fun_app _ | Bool_elim _ ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm Env.empty


module Semantics = struct

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (vtm -> vtm)
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of Level.t              (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim (head : vtm) (vtm1 : unit -> vtm) (vtm2 : unit -> vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> vtm1 ()
    | Bool_lit false -> vtm2 ()
    | _ -> invalid_arg "expected boolean"

  let rec eval (env : vtm Env.t) (tm : tm) : vtm =
    match tm with
    | Var i -> Env.lookup i env
    | Ann (tm, _) -> eval env tm
    | Let (_, _, def, body) -> eval (Env.extend (eval env def) env) body
    | Fun_lit (x, param_ty, body) -> Fun_lit (x, param_ty, fun v -> eval (Env.extend v env) body)
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        let head = eval env head in
        let vtm1 () = eval env tm1 in
        let vtm2 () = eval env tm2 in
        bool_elim head vtm1 vtm2

  let rec quote (size : Size.t) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm size ntm
    | Fun_lit (x, param_ty, body) ->
        Fun_lit (x, param_ty, quote (Size.succ size) (body (Neu (Var (Level.next size)))))
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
  and quote_ntm (size : Size.t) (ntm : ntm) : tm =
    match ntm with
    | Var l ->
        Var (Level.to_index size l)
    | Fun_app (head, arg) ->
        Fun_app (quote_ntm size head, quote size arg)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote size (vtm1 ()) in
        let tm2 = quote size (vtm2 ()) in
        Bool_elim (quote_ntm size head, tm1, tm2)

  let normalise (env : vtm Env.t) (tm : tm) : tm =
    quote (Env.size env) (eval env tm)
    [@@warning "-unused-value-declaration"]

end


(* Typing context *)

type context = {
  size : Size.t;
  bindings : ty Env.t;
}

let empty = {
  size = Size.zero;
  bindings = Env.empty;
}

let add_bind (ty : ty) (ctx : context) = {
  size = Size.succ ctx.size;
  bindings = Env.extend ty ctx.bindings;
}


(* Elaboration effect *)

type 'a elab = context -> 'a

type ('a, 'e) elab_err = ('a, 'e) result elab

let run (type a) (elab : a elab) : a =
  elab empty


(* Forms of Judgement *)

type var = Level.t

type check_tm = ty -> tm elab
type infer_tm = (tm * ty) elab

type 'e check_tm_err = ty -> (tm, 'e) elab_err
type 'e infer_tm_err = (tm * ty, 'e) elab_err


(* Error handling *)

type ty_mismatch = {
  found_ty : ty;
  expected_ty : ty;
}

let fail (type a e) (e : e) : (a, e) elab_err =
  fun _ ->
    Error e

let catch_check_tm (type e) (f : e -> check_tm) (elab : e check_tm_err) : check_tm =
  fun ty ctx ->
    match elab ty ctx with
    | Ok x -> x
    | Error e -> f e ty ctx

let catch_infer_tm (type e) (f : e -> infer_tm) (elab : e infer_tm_err) : infer_tm =
  fun ctx ->
    match elab ctx with
    | Ok x -> x
    | Error e -> f e ctx


let ( let* ) = Result.bind


(** Directional rules *)

let conv (elab : infer_tm) : [> `Type_mismatch of ty_mismatch] check_tm_err =
  fun expected_ty ctx ->
    let tm, found_ty = elab ctx in
    match expected_ty = found_ty with
    | true -> Ok tm
    | false -> Error (`Type_mismatch { found_ty; expected_ty })

let ann (elab : check_tm) (ty : ty) : infer_tm =
  fun ctx ->
    let tm = elab ty ctx in
    Ann (tm, ty), ty


(* Structural rules *)

let var (l : var) : [> `Unbound_var] infer_tm_err =
  fun ctx ->
    let i = Level.to_index ctx.size l in
    match Env.lookup_opt i ctx.bindings with
    | Some ty -> Ok (Var i, ty)
    | None -> Error `Unbound_var

let let_synth (name, def_ty, def : name * ty * check_tm) (body : var -> infer_tm) : infer_tm =
  fun ctx ->
    let def = def def_ty ctx in
    let body, body_ty = body (Level.next ctx.size) (add_bind def_ty ctx) in
    Let (name, def_ty, def, body), body_ty

let let_check (name, def_ty, def : name * ty * check_tm) (body : var -> check_tm) : check_tm =
  fun body_ty ctx ->
    let def = def def_ty ctx in
    let body = body (Level.next ctx.size) body_ty (add_bind def_ty ctx) in
    Let (name, def_ty, def, body)


(* Type connectives *)

module Fun = struct

  let form (param_ty : ty) (body_ty : ty) : ty =
    Fun_type (param_ty, body_ty)

  let intro_check (name, param_ty : name * ty option) (body : var -> check_tm) :  [> `Mismatched_param_ty of ty_mismatch | `Unexpected_fun_lit of ty] check_tm_err =
    fun fun_ty ctx ->
      match param_ty, fun_ty with
      | None, Fun_type (param_ty, body_ty) ->
          let body = body (Level.next ctx.size) body_ty (add_bind param_ty ctx) in
          Ok (Fun_lit (name, param_ty, body) : tm)
      | Some param_ty, Fun_type (param_ty', body_ty) ->
          if param_ty = param_ty' then
            let body = body (Level.next ctx.size) body_ty (add_bind param_ty ctx) in
            Ok (Fun_lit (name, param_ty, body))
          else
            Error (`Mismatched_param_ty { found_ty = param_ty; expected_ty = param_ty' })
      | _ ->
          Error (`Unexpected_fun_lit fun_ty)

  let intro_synth (name, param_ty : name * ty) (body : var -> infer_tm) : infer_tm =
    fun ctx ->
      let body, body_ty = body (Level.next ctx.size) (add_bind param_ty ctx) in
      Fun_lit (name, param_ty, body), Fun_type (param_ty, body_ty)

  let elim (head : infer_tm) (arg : infer_tm) : [> `Unexpected_arg of ty  | `Type_mismatch of ty_mismatch] infer_tm_err =
    fun ctx ->
      match head ctx with
      | head, Fun_type (param_ty, body_ty) ->
          let* arg = conv arg param_ty ctx in
          Ok (Fun_app (head, arg), body_ty)
      | _, head_ty ->
          Error (`Unexpected_arg head_ty)

end


module Int = struct

  let form : ty = Int_type

  let intro (i : int) : infer_tm =
    fun _ -> (Int_lit i, Int_type)

end


module Bool = struct

  let form : ty = Bool_type

  let intro_true : infer_tm =
    fun _ -> (Bool_lit true, Bool_type)

  let intro_false : infer_tm =
    fun _ -> (Bool_lit false, Bool_type)

  let elim_check (head : check_tm) (tm1 : check_tm) (tm2 : check_tm) : check_tm =
    fun ty ctx ->
      let head = head Bool_type ctx in
      let tm1 = tm1 ty ctx in
      let tm2 = tm2 ty ctx in
      Bool_elim (head, tm1, tm2)

  let elim_synth (head : check_tm) (tm1 : infer_tm) (tm2 : infer_tm) : [`Mismatched_branches of ty_mismatch] infer_tm_err =
    fun ctx ->
      let head = head Bool_type ctx in
      let tm1, ty1 = tm1 ctx in
      let tm2, ty2 = tm2 ctx in
      match ty1 = ty2 with
      | true -> Ok (Bool_elim (head, tm1, tm2), ty1)
      | false -> Error (`Mismatched_branches { found_ty = ty2; expected_ty = ty1 })

end
