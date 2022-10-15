module LSystem = FractalGrowth.LSystem


(** Example L-Systems that can be run via the CLI *)
module Examples : sig

  val list : (string * (unit -> unit)) list

end = struct

  module Make (G : LSystem.Grammar) = struct

    module Util = LSystem.Util (G)

    (** Print an infinite series of generations for the system *)
    let print_generations () =
      Util.generate G.axiom
        |> Seq.map Util.string_of_word
        |> Seq.iter print_endline

  end

  module Examples = FractalGrowth.Examples

  module Algae = Make (Examples.Algae)
  module Filament = Make (Examples.Filament)
  module KochIsland = Make (Examples.KochIsland)
  module Parametric = Make (Examples.Parametric)
  module BinaryTree = Make (Examples.BinaryTree)
  module CantorSet = Make (Examples.CantorSet)

  let list = [
    "algae", Algae.print_generations;
    "filament", Filament.print_generations;
    "koch-island", KochIsland.print_generations;
    "parametric", Parametric.print_generations;
    "binary-tree", BinaryTree.print_generations;
    "cantor-set", CantorSet.print_generations;
  ]

end


(** {1 Subcommands} *)

let generations_cmd name =
  match Examples.list |> List.assoc_opt name with
  | None -> Printf.eprintf "unkown system"
  | Some run -> run ()

let list_cmd () =
  Examples.list |> List.iter (fun (name, _) ->
    print_endline name)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let system : string Term.t =
    Arg.(required
      & opt (some & string) None
      & info ["system"] ~docv:"SYSTEM" ~doc:"The system to run.")
  in

  Cmd.group (Cmd.info "fractal-growth") [
    Cmd.v (Cmd.info "generations" ~doc:"print the generations of a system to standard output")
      Term.(const generations_cmd $ system);
    Cmd.v (Cmd.info "list" ~doc:"list the available systems")
      Term.(const list_cmd $ const ());
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
