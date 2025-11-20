type ty =
  | Fun_ty of ty * ty
  | Text_ty
  | List_ty of ty
  | Bool_ty
  | Int_ty
  (* | Node_ty (* TODO: More precise node types *) *)

type tm =
  | Var of string
  | Let of string * ty * tm * tm
  | Fun_lit of string * ty * tm
  | Fun_app of tm * tm
  | Text_lit of string
  | List_nil
  | List_cons of tm * tm
  (* | List_elim of tm * tm * (string * string * tm) *)
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Int_lit of int
  (* | Node_lit of (string * tm) list * tm list *)
  (* | Node_elim of ... *)
  | Prim_app of Prim.t * tm list

module Semantics = struct

  (** {2 Semantic domain} *)

  type vtm =
    | Fun_lit of string * ty * (vtm -> vtm)
    | List_nil
    | List_cons of vtm * vtm
    | Text_lit of string
    | Bool_lit of bool
    | Int_lit of int
    (* | Node_lit of (string * vtm) list * vtm list *)

  type env = (string * vtm) list

  (** {2 Evaluation} *)

  let rec eval (locals : env) (tm : tm) : vtm =
    match tm with
    | Var name ->
        List.assoc name locals
    | Let (name, _, def, body) ->
        eval ((name, eval locals def) :: locals) body
    | Fun_lit (name, ty, body) ->
        Fun_lit (name, ty, fun v -> eval ((name, v) :: locals) body)
    | Fun_app (head, arg) ->
        begin match eval locals head with
        | Fun_lit (_, _, f) -> f (eval locals arg)
        | _ -> invalid_arg "expected function literal"
        end
    | List_nil -> List_nil
    | List_cons (tm, tms) ->
        List_cons (eval locals tm, eval locals tms)
    | Text_lit s ->
        Text_lit s
    | Bool_lit b ->
        Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        begin match eval locals head with
        | Bool_lit true -> eval locals tm1
        | Bool_lit false -> eval locals tm2
        | _ -> invalid_arg "expected boolean literal"
        end
    | Int_lit n ->
        Int_lit n
    | Prim_app (prim, args) ->
        let args =
          args |> List.map @@ fun arg : Prim.value ->
            match eval locals arg with
            | Text_lit s -> Text_lit s
            | Int_lit n -> Int_lit n
            | Bool_lit b -> Bool_lit b
            | _ -> failwith "expected primitive"
        in
        begin match Prim.app prim args with
        | Text_lit s -> Text_lit s
        | Int_lit n -> Int_lit n
        | Bool_lit b -> Bool_lit b
        end

end
