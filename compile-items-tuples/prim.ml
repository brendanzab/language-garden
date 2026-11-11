(** Primitive values and operations *)

module Ty = struct

  type t =
    | Bool
    | I32

end

module Value = struct

  type t =
    | Bool of bool
    | I32 of int32

end

module Op = struct

  type t =
    | Bool_eq
    | I32_eq
    | I32_add
    | I32_sub
    | I32_mul
    | I32_neg

  let ty (op : t) : Ty.t Iarray.t * Ty.t =
    match op with
    | Bool_eq -> Ty.[|Bool; Bool|], Ty.Bool
    | I32_eq -> Ty.[|I32; I32|], Ty.Bool
    | I32_add -> Ty.[|I32; I32|], Ty.I32
    | I32_sub -> Ty.[|I32; I32|], Ty.I32
    | I32_mul -> Ty.[|I32; I32|], Ty.I32
    | I32_neg -> Ty.[|I32|], Ty.I32

  let app (op : t) (args : Value.t Iarray.t) : Value.t =
    match op, args with
    | Bool_eq, Value.[|Bool bool1; Bool bool2|] -> Value.Bool (Bool.equal bool1 bool2)
    | I32_eq, Value.[|I32 int1; I32 int2|] -> Value.Bool (Int32.equal int1 int2)
    | I32_add, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.add int1 int2)
    | I32_sub, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.sub int1 int2)
    | I32_mul, Value.[|I32 int1; I32 int2|] -> Value.I32 (Int32.mul int1 int2)
    | I32_neg, Value.[|I32 int|] -> Value.I32 (Int32.neg int)
    | _, _ -> failwith "Prim.Op.app"

  let name (op : t) : string =
    match op with
    | Bool_eq -> "bool-eq"
    | I32_eq -> "i32-eq"
    | I32_add -> "i32-add"
    | I32_sub -> "i32-sub"
    | I32_mul -> "i32-mul"
    | I32_neg -> "i32-neg"

  let pp (op : t) (ppf : Format.formatter) =
    Format.fprintf ppf "#%s" (name op)

  let lookup (name : string) : t option =
    match name with
    | "bool-eq" -> Some Bool_eq
    | "i32-eq" -> Some I32_eq
    | "i32-add" -> Some I32_add
    | "i32-sub" -> Some I32_sub
    | "i32-mul" -> Some I32_mul
    | "i32-neg" -> Some I32_neg
    | _ -> None

end
