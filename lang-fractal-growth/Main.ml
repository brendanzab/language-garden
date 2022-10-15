module LSystem = LSystem


module Runner (System : LSystem.S) = struct

  open LSystem.Util (System)

  (** Print an infinite series of generations for the system *)
  let print_generations () =
    generate System.axiom
      |> Seq.map string_of_word
      |> Seq.iter (Printf.printf "  %s\n");

end

let systems =
  let module Algae = Runner (Examples.Algae) in
  let module Filament = Runner (Examples.Filament) in
  let module KochIsland = Runner (Examples.KochIsland) in

  [
    "algae", Algae.print_generations;
    "filament", Filament.print_generations;
    "koch-island", KochIsland.print_generations;
  ]


(** {1 Subcommands} *)

let generations_cmd name =
  match systems |> List.assoc_opt name with
  | None -> Printf.eprintf "unkown system"
  | Some run -> run ()

let list_cmd () =
  systems |> List.iter (fun (name, _) ->
    print_endline name)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let system : string Term.t =
    Arg.(required
      & opt (some & string) None
      & info ["system"] ~docv:"SYSTEM"
          ~doc:"The system to run.")
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
