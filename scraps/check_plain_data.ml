(** A simple implementation of type inference for JSON-style objects.

    Based on {{: https://www.haskellforall.com/2025/08/type-inference-for-plain-data.html}
    “Type inference for plain data”} by Gabriella Gonzalez.
*)

module Key_map = Map.Make (String)

module Type = struct

  type t =
    | Object of (t Key_map.t)   (* { "key₀": type₀, "key₁": type₁, … } *)
    | Array of t                (* type[] *)
    | String                    (* string *)
    | Number                    (* number *)
    | Bool                      (* boolean *)
    | Option of t               (* null | type *)
    | Never                     (* never, the subtype of all other types *)
    | Any                       (* any, the supertype of all other types *)

  let rec unify (t1 : t) (t2 : t) : t =
    match (t1, t2) with
    | String, String -> String
    | Number, Number -> Number
    | Bool, Bool -> Bool

    | Never, t | t, Never -> t
    | Array t1, Array t2 -> Array (unify t1 t2)
    | Option t1, Option t2 | Option t1, t2 | t1, Option t2 -> Option (unify t1 t2)

    | Object ts1, Object ts2 ->
        let unify_fields = Key_map.merge @@ fun _ t1 t2 ->
          match t1, t2 with
          | Some t, None | None, Some t -> Some (Option t)
          | Some t1, Some t2 -> Some (unify t1 t2)
          | None, None -> None
        in
        Object (unify_fields ts1 ts2)

    | _, _ -> Any

end

module Value = struct

  [@@@warning "-unused-constructor"]
  [@@@warning "-unused-value-declaration"]

  type t =
    | String of string
    | Bool of bool
    | Number of float
    | Null
    | Object of t Key_map.t
    | Array of t array

  let rec infer (v : t) : Type.t =
    match v with
    | String _ -> Type.String
    | Bool _ -> Type.Bool
    | Number _ -> Type.Number
    | Null -> Type.Never
    | Object fields -> Type.Object (Key_map.map infer fields)
    | Array elems -> Type.Array (elems |> Array.map infer |> Array.fold_left Type.unify Type.Never)

end
