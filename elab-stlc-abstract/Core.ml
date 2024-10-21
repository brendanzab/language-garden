open DeBruijn

(* Syntax *)

type name = string

type ty =
  | A
  | B
  | C
  | FunTy of ty * ty

type tm =
  | Var of Index.t
  | Ann of tm * ty
  | Let of name * ty * tm * tm
  | FunLit of name * ty * tm
  | FunApp of tm * tm


(* Pretty printing *)

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | FunTy (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | A -> Format.fprintf fmt "A"
  | B -> Format.fprintf fmt "B"
  | C -> Format.fprintf fmt "C"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann (fmt : Format.formatter) (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param (fmt : Format.formatter) (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let pp_tm (fmt : Format.formatter) (tm : tm) : unit =
  let rec pp_tm (names : string Env.t) (fmt : Format.formatter) (tm : tm) : unit =
    match tm with
    | Let _ as tm ->
        let rec go names fmt = function
          | Let (name, def_ty, def, body) ->
              Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
                pp_name_ann (name, def_ty)
                (pp_tm names) def
                (go (Env.extend name names)) body
          | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
        in
        Format.fprintf fmt "@[<v>%a@]" (go names) tm
  | FunLit _ as tm ->
      let rec go names fmt tm =
        match tm with
        | FunLit (name, param_ty, (FunLit _ as body)) ->
            Format.fprintf fmt "@[fun@ %a@ =>@]@ %a"
              pp_param (name, param_ty)
              (go (Env.extend name names)) body
        | FunLit (name, param_ty, body) ->
            Format.fprintf fmt "@[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (Env.extend name names)) body
        | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv 2>@[<hv>%a" (go names) tm
    | tm ->
        pp_app_tm names fmt tm
  and pp_app_tm names fmt tm =
    match tm with
    | FunApp (head, arg) ->
        Format.fprintf fmt "@[%a@ %a@]"
          (pp_app_tm names) head
          (pp_atomic_tm names) arg
    | tm ->
        pp_atomic_tm names fmt tm
  and pp_atomic_tm names fmt tm =
    match tm with
    | Var index ->
        Format.fprintf fmt "%s" (Env.lookup index names)
    | tm ->
        Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
  in
  pp_tm Env.empty fmt tm


module Semantics = struct

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | FunLit of name * ty * (vtm -> vtm)

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of Level.t              (* A fresh variable (used when evaluating under a binder) *)
    | FunApp of ntm * vtm

  let rec eval (vtms : vtm Env.t) (tm : tm) : vtm =
    match tm with
    | Var i -> Env.lookup i vtms
    | Ann (tm, _) -> eval vtms tm
    | Let (_, _, def_tm, body_tm) -> eval (Env.extend (eval vtms def_tm) vtms) body_tm
    | FunLit (x, param_ty, body_tm) -> FunLit (x, param_ty, fun v -> eval (Env.extend v vtms) body_tm)
    | FunApp (head_tm, arg_tm) -> begin
        match eval vtms head_tm with
        | FunLit (_, _, body) -> body (eval vtms arg_tm)
        | Neu ntm -> Neu (FunApp (ntm, eval vtms arg_tm))
    end

  let rec quote (size : Size.t) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm size ntm
    | FunLit (x, param_ty, body) ->
        FunLit (x, param_ty, quote (Size.succ size) (body (Neu (Var (Level.next size)))))
  and quote_ntm (size : Size.t) (ntm : ntm) : tm =
    match ntm with
    | Var l -> Var (Level.to_index size l)
    | FunApp (head, arg) -> FunApp (quote_ntm size head, quote size arg)

  let normalise (vtms : vtm Env.t) (tm : tm) : tm =
    quote (Env.size vtms) (eval vtms tm)
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

type check = ty -> tm elab
type synth = (tm * ty) elab

type 'e check_err = ty -> (tm, 'e) elab_err
type 'e synth_err = (tm * ty, 'e) elab_err


(* Error handling *)

type ty_mismatch = {
  found_ty : ty;
  expected_ty : ty;
}

let fail (type a e) (e : e) : (a, e) elab_err =
  fun _ ->
    Error e

let catch_check (type e) (f : e -> check) (elab : e check_err) : check =
  fun ty ctx ->
    match elab ty ctx with
    | Ok x -> x
    | Error e -> f e ty ctx

let catch_synth (type e) (f : e -> synth) (elab : e synth_err) : synth =
  fun ctx ->
    match elab ctx with
    | Ok x -> x
    | Error e -> f e ctx


let ( let* ) = Result.bind


(** Directional rules *)

let conv (elab : synth) : [> `TypeMismatch of ty_mismatch] check_err =
  fun expected_ty ctx ->
    let tm, found_ty = elab ctx in
    match expected_ty = found_ty with
    | true -> Ok tm
    | false -> Error (`TypeMismatch { found_ty; expected_ty })

let ann (elab : check) (ty : ty) : synth =
  fun ctx ->
    let tm = elab ty ctx in
    Ann (tm, ty), ty


(* Structural rules *)

let var (l : var) : [> `UnboundVar] synth_err =
  fun ctx ->
    let i = Level.to_index ctx.size l in
    match Env.lookup_opt i ctx.bindings with
    | Some ty -> Ok (Var i, ty)
    | None -> Error `UnboundVar

let let_synth (def_n, def_ty, def_elab : name * ty * check) (body_elab : var -> synth) : synth =
  fun ctx ->
    let def_tm = def_elab def_ty ctx in
    let body_tm, body_ty = body_elab (Level.next ctx.size) (add_bind def_ty ctx) in
    Let (def_n, def_ty, def_tm, body_tm), body_ty

let let_check (def_n, def_ty, def_elab : name * ty * check) (body_elab : var -> check) : check =
  fun body_ty ctx ->
    let def_tm = def_elab def_ty ctx in
    let body_tm = body_elab (Level.next ctx.size) body_ty (add_bind def_ty ctx) in
    Let (def_n, def_ty, def_tm, body_tm)


(** Function rules *)

let fun_intro_check (param_n, param_ty : name * ty option) (body_elab : var -> check) :  [> `MismatchedParamTy of ty_mismatch | `UnexpectedFunLit of ty] check_err =
  fun fun_ty ctx ->
    match param_ty, fun_ty with
    | None, FunTy (param_ty, body_ty) ->
        let body_tm = body_elab (Level.next ctx.size) body_ty (add_bind param_ty ctx) in
        Ok (FunLit (param_n, param_ty, body_tm) : tm)
    | Some param_ty, FunTy (param_ty', body_ty) ->
        if param_ty = param_ty' then
          let body_tm = body_elab (Level.next ctx.size) body_ty (add_bind param_ty ctx) in
          Ok (FunLit (param_n, param_ty, body_tm))
        else
          Error (`MismatchedParamTy { found_ty = param_ty; expected_ty = param_ty' })
    | _ ->
        Error (`UnexpectedFunLit fun_ty)

let fun_intro_synth (param_n, param_ty : name * ty) (body_elab : var -> synth) : synth =
  fun ctx ->
    let body_tm, body_ty = body_elab (Level.next ctx.size) (add_bind param_ty ctx) in
    FunLit (param_n, param_ty, body_tm), FunTy (param_ty, body_ty)

let fun_elim (head_elab : synth) (arg_elab : synth) : [> `UnexpectedArg of ty  | `TypeMismatch of ty_mismatch] synth_err =
  fun ctx ->
    match head_elab ctx with
    | head_tm, FunTy (param_ty, body_ty) ->
        let* arg_tm = conv arg_elab param_ty ctx in
        Ok (FunApp (head_tm, arg_tm), body_ty)
    | _, head_ty ->
        Error (`UnexpectedArg head_ty)
