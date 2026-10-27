(** Compile arithmetic expressions to stack machine instructions using lists,
    difference lists, and dynamic arrays. *)

(** Nested, tree-style arithmetic expressions *)
module Tree = struct

  type expr =
    | Int of int
    | Add of expr * expr
    | Mul of expr * expr

end

(** Arithmetic expressions as lists of stack machine instructions *)
module Stack = struct

  type instr =
    | Int of int
    | Add
    | Mul

  type expr = instr list

end

(** Compile naively using list concatenation directly, which is expensive. *)
module Compile_list = struct

  let rec compile_expr (e : Tree.expr) : Stack.expr =
    match e with
    | Tree.Int i -> [Stack.Int i]
    | Tree.Add (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Stack.Add]
    | Tree.Mul (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Stack.Mul]

end

(** Compile using difference lists. *)
module Compile_diff_list = struct

  (** Difference lists, supporting O(1) list concatenation.

      - Hughes, 1986. A novel representation of lists and its application to the function “reverse”.
        https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf
      - https://hackage-content.haskell.org/package/dlist-1.0/docs/Data-DList.html
  *)
  module Diff_list : sig

    type 'a t

    val nil : 'a t
    val cons : 'a -> 'a t -> 'a t
    val singleton : 'a -> 'a t
    val append : 'a t -> 'a t -> 'a t

    val of_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list

  end = struct

    type 'a t = 'a list -> 'a list

    let nil = Fun.id
    let cons x xs = Fun.compose (List.cons x) xs
    let singleton = List.cons
    let append = Fun.compose

    let of_list = List.append
    let to_list xs = xs []

  end

  let compile_expr (e : Tree.expr) : Stack.expr =
    let module D = Diff_list in
    let ( @ ) = Diff_list.append in

    let rec compile_expr (e : Tree.expr) =
      match e with
      | Tree.Int i -> D.singleton (Stack.Int i)
      | Tree.Add (e1, e2) -> compile_expr e1 @ compile_expr e2 @ D.singleton (Stack.Add)
      | Tree.Mul (e1, e2) -> compile_expr e1 @ compile_expr e2 @ D.singleton (Stack.Mul)
    in

    compile_expr e |> D.to_list

end

(** Compile by pushing to a dynamically resizable array. This should avoid the
    indirect calls involved when using difference lists. *)
module Compile_dynarray = struct

  let compile_expr (e : Tree.expr) : Stack.expr =
    let instrs = Dynarray.create () in
    let emit_instr = Dynarray.add_last instrs in

    let rec compile_expr (e : Tree.expr) =
      match e with
      | Tree.Int i -> emit_instr (Stack.Int i)
      | Tree.Add (e1, e2) -> compile_expr e1; compile_expr e2; emit_instr Stack.Add;
      | Tree.Mul (e1, e2) -> compile_expr e1; compile_expr e2; emit_instr Stack.Mul;
    in

    compile_expr e;
    Dynarray.to_list instrs

end


(* Test that the implementations are equivalent *)
let () = begin

  Printexc.record_backtrace true;

  let expr = Tree.Int 1 in

  assert (Compile_list.compile_expr expr = Compile_diff_list.compile_expr expr);
  assert (Compile_list.compile_expr expr = Compile_dynarray.compile_expr expr);

  let expr = Tree.(Add (Int 2, Mul (Int 3, Int 4))) in

  assert (Compile_list.compile_expr expr = Compile_diff_list.compile_expr expr);
  assert (Compile_list.compile_expr expr = Compile_dynarray.compile_expr expr);

end
