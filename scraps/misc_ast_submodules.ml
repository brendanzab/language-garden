(** OCaml’s mutually recursive modules are often quite tedious to use, requiring
    explicit module signatures. This can be quite frustrating if you want to
    define an AST with submodules for each node type.

    This file shows a pattern for nesting mutually recursive datatypes in
    submodules without duplicating the datatype definitions. It is based on
    {{: https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/}
    this blog post}.
*)

module rec Expr : sig

  type t =
    | Var of string
    | Zero
    | Succ of Expr.t
    | Block of Stmt.t list

  val pp : Format.formatter -> t -> unit

end = struct

  include Expr
  (* Derive the definitions from the signature.

     NOTE: If we forget to define [Expr.pp], we will get an [unused-value-declaration]
     warning, which if ignored will result in a fatal runtime error if [Expr.pp]
     is ever executed, for example:

     Fatal error: exception Undefined_recursive_module("scraps/lang_ast_submodules.ml", 19, 6)
  *)

  let pp ppf expr =
    match expr with
    | Var x -> Format.fprintf ppf "%s" x
    | Zero -> Format.fprintf ppf "Z"
    | Succ expr -> Format.fprintf ppf "S(%a)" Expr.pp expr
    | Block stmts ->
        let pp_sep ppf () = Format.fprintf ppf ";@ " in
        Format.fprintf ppf "@[<hv>{@ %a@ }@]"
          (Format.pp_print_list ~pp_sep Stmt.pp) stmts

end

and Stmt : sig

  type t =
    | Let of string * Expr.t
    | Print of Expr.t

  val pp : Format.formatter -> t -> unit

end = struct

  include Stmt

  let pp ppf stmt =
    match stmt with
    | Let (x, expr) -> Format.fprintf ppf "@[<2>@[let@ %s :=@]@ %a@]" x Expr.pp expr
    | Print expr -> Format.fprintf ppf "@[print@ %a@]" Expr.pp expr

end

(*
let () = begin
  Format.printf "%a\n"
    Expr.pp
    (Block [
      Print Zero;
      Let ("x", (Succ (Succ Zero)));
      Print (Succ (Succ Zero));
      Print (Var "x");
    ]);
end
*)
