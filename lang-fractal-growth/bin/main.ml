module System = Fractal_growth.System
module Systems = Fractal_growth.Systems


(** {1 Helper functions} *)

module type Printable_grammar = sig
  include System.Grammar
  val string_of_word : symbol list -> string
end

let print_generations (module G : Printable_grammar) =
  let module Util = System.Util (G) in
  Util.generate G.axiom
    |> Seq.map G.string_of_word
    |> Seq.iter print_endline

(** Example L-Systems that can be run via the CLI *)
let systems : (string * (module Printable_grammar)) list = [
  "algae", (module Systems.Algae);
  "filament", (module Systems.Filament);
  "koch-island", (module Systems.Koch_island);
  "parametric", (module Systems.Parametric);
  "binary-tree", (module Systems.Binary_tree);
  "cantor-set", (module Systems.Cantor_set);
  "monopodial-inflorence", (module Systems.Monopodial_inflorence);
]


(** {1 Subcommands} *)

let generations_cmd name =
  match systems |> List.assoc_opt name with
  | None -> Printf.eprintf "unkown system '%s'" name; exit 1
  | Some system -> print_generations system

let list_cmd () =
  systems |> List.iter (fun (name, _) ->
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
