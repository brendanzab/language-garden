type name = string


(* Variables *)

(** {i De Bruijn index} that represents a variable occurance by the number of
    binders between the occurance and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurance by the number of
    binders from the top of the environment to the binder that the ocurrance
    refers to.

    These do not change their meaning as new bindings are added to the
    environment.
*)
type level = int

(** [level_to_index size l] converts a {!level} to an {!index} bound in an
    environment of length [size]. Assumes that [size > level]. *)
let level_to_index (size : int) (l : level) =
  size - l - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(* Syntax *)

type ty =
  | A
  | B
  | C
  | FunTy of ty * ty

type tm =
  | Var of index
  | Ann of tm * ty
  | Let of name * ty * tm * tm
  | FunLit of name * ty * tm
  | FunApp of tm * tm


(* Pretty printing *)

let rec pp_ty (fmt : Format.formatter) : ty -> unit =
  function
  | FunTy (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt =
  function
  | A -> Format.fprintf fmt "A"
  | B -> Format.fprintf fmt "B"
  | C -> Format.fprintf fmt "C"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let pp_tm (fmt : Format.formatter) (tm : tm) : unit =
  let rec pp_tm (names : string env) (fmt : Format.formatter) (tm : tm) : unit =
    match tm with
    | Let _ as tm ->
        let rec go names fmt = function
          | Let (name, def_ty, def, body) ->
              Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
                pp_name_ann (name, def_ty)
                (pp_tm names) def
                (go (name :: names)) body
          | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
        in
        go names fmt tm
    | FunLit (name, param_ty, body) ->
        Format.fprintf fmt "@[@[fun@ %a@ =>@]@ %a@]"
          pp_param (name, param_ty)
          (pp_tm (name :: names)) body
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
        Format.fprintf fmt "%s" (List.nth names index)
    | tm ->
        Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
  in
  pp_tm [] fmt tm


module Semantics = struct

  type vtm =
    | Neu of ntm
    | FunLit of name * ty * (vtm -> vtm)
  and ntm =
    | Var of level
    | FunApp of ntm * vtm

  let rec eval (vtms : vtm env) (tm : tm) : vtm =
    match tm with
    | Var i -> List.nth vtms i
    | Ann (tm, _) -> eval vtms tm
    | Let (_, _, def_tm, body_tm) -> eval (eval vtms def_tm :: vtms) body_tm
    | FunLit (x, param_ty, body_tm) -> FunLit (x, param_ty, fun v -> eval (v :: vtms) body_tm)
    | FunApp (head_tm, arg_tm) -> begin
        match eval vtms head_tm with
        | FunLit (_, _, body) -> body (eval vtms arg_tm)
        | Neu ntm -> Neu (FunApp (ntm, eval vtms arg_tm))
    end

  let rec quote (size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm size ntm
    | FunLit (x, param_ty, body) -> FunLit (x, param_ty, quote (size + 1) (body (Neu (Var size))))
  and quote_ntm (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var l -> Var (level_to_index size l)
    | FunApp (head, arg) -> FunApp (quote_ntm size head, quote size arg)

  let normalise (vtms : vtm env) (tm : tm) : tm =
    quote (List.length vtms) (eval vtms tm)
    [@@ warning "-unused-value-declaration"]

end


(* Typing context *)

type context = {
  size : int;
  bindings : ty env;
}

let empty = {
  size = 0;
  bindings = [];
}

let add_bind (ty : ty) (ctx : context) = {
  size = ctx.size + 1;
  bindings = ty :: ctx.bindings;
}


(* Elaboration effect *)

type 'a elab = context -> 'a

let run (elab : 'a elab) : 'a =
  elab empty


(* Error handling *)

exception UnboundVar
exception UnexpectedFunLit
exception UnexpectedArg of { head_ty : ty }
exception TypeMismatch of { found_ty : ty; expected_ty : ty }

let fail (e : exn) : 'a elab =
  fun _ ->
    raise e

let handle (f : exn -> 'a elab option) (elab : 'a elab) : 'a elab  =
  fun ctx ->
    try elab ctx with
    | e -> begin
        match f e with
        | Some elab' -> elab' ctx
        | None -> raise e
    end


(* Forms of Judgement *)

type var = level

type check = ty -> tm elab
type synth = (tm * ty) elab


(** Directional rules *)

let conv (elab : synth) : check =
  fun ty ctx ->
    let tm, ty' = elab ctx in
    match ty = ty' with
    | true -> tm
    | false -> raise (TypeMismatch { found_ty = ty'; expected_ty = ty })

let ann (elab : check) (ty : ty) : synth =
  fun ctx ->
    let tm = elab ty ctx in
    Ann (tm, ty), ty


(* Structural rules *)

let var (l : var) : synth =
  fun ctx ->
    let i = level_to_index ctx.size l in
    match List.nth_opt ctx.bindings i with
    | Some ty -> Var i, ty
    | None -> raise UnboundVar

let let_synth (def_n, def_ty, def_elab : name * ty * check) (body_elab : var -> synth) : synth =
  fun ctx ->
    let def_tm = def_elab def_ty ctx in
    let body_tm, body_ty = body_elab ctx.size (add_bind def_ty ctx) in
    Let (def_n, def_ty, def_tm, body_tm), body_ty

let let_check (def_n, def_ty, def_elab : name * ty * check) (body_elab : var -> check) : check =
  fun body_ty ctx ->
    let def_tm = def_elab def_ty ctx in
    let body_tm = body_elab ctx.size body_ty (add_bind def_ty ctx) in
    Let (def_n, def_ty, def_tm, body_tm)


(** Function rules *)

let fun_intro_check (param_n : name) (body_elab : var -> check) : check =
  fun ty ctx ->
    match ty with
    | FunTy (param_ty, body_ty) ->
        let body_tm = body_elab ctx.size body_ty (add_bind param_ty ctx) in
        FunLit (param_n, param_ty, body_tm)
    | _ ->
        raise UnexpectedFunLit

let fun_intro_synth (param_n, param_ty : name * ty) (body_elab : var -> synth) : synth =
  fun ctx ->
    let body_tm, body_ty = body_elab ctx.size (add_bind param_ty ctx) in
    FunLit (param_n, param_ty, body_tm),
    FunTy (param_ty, body_ty)

let fun_elim (head_elab : synth) (arg_elab : synth) : synth =
  fun ctx ->
    match head_elab ctx with
    | head_tm, FunTy (param_ty, body_ty) ->
        let arg_tm = conv arg_elab param_ty ctx in
        FunApp (head_tm, arg_tm), body_ty
    | _, head_ty ->
        raise (UnexpectedArg { head_ty })
