(** OCaml’s mutually recusive modules are often quite tedious to use, requiring
    explicit module signatures. This can be quite frustrating if you want to
    define an AST with submodules for each node type.

    This file shows a pattern for nesting mutually recursive datatypes in
    submodules without duplicating the datatype definitions. It is based on
    {{:https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/}
    this blog post}.
*)

module rec Expr : sig

  type t =
    | Zero
    | Succ of Expr.t
    | Block of Stmt.t list

  val pp : Format.formatter -> t -> unit

end = struct

  include Expr
  (* Infer the definitions from the signture.

     NOTE: If we forget to define [Expr.pp], we will get an [unused-value-declaration]
     warning, which if ignored will result in a fatal runtime error if [Expr.pp]
     is ever executed, for example:

     Fatal error: exception Undefined_recursive_module("scraps/lang_ast_submodules.ml", 19, 6)
  *)

  let pp ppf expr =
    match expr with
    | Zero -> Format.fprintf ppf "Z"
    | Succ expr -> Format.fprintf ppf "S(%a)" Expr.pp expr
    | Block stmts ->
        let pp_sep ppf () = Format.fprintf ppf "@ " in
        Format.fprintf ppf "@[<hv>{@ %a@ }@]"
          (Format.pp_print_list ~pp_sep Stmt.pp) stmts

end

and Stmt : sig

  type t =
    | Print of Expr.t

  val pp : Format.formatter -> t -> unit

end = struct

  include Stmt

  let pp ppf stmt =
    match stmt with
    | Print expr -> Format.fprintf ppf "print %a" Expr.pp expr

end

let () = begin
  Format.printf "%a\n"
    Expr.pp
    (Block [
      Print Zero;
      Print (Succ (Succ Zero));
      Print (Succ Zero);
    ]);
end
