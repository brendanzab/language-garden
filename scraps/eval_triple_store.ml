(** An example of inferring facts from a triple store.

    This is a port of {{: https://gist.github.com/jackrusher/5018122}
    triples.clj} by Jack Rusher.

    Resources:

    - {{: https://berlin.social/@jack/113305897524830516} Original thread} by Jack Rusher
    - {{: https://gist.github.com/jackrusher/5018122} triples.clj}
      “A super simple example of inference from a set of triples”
    - {{: https://gist.github.com/jackrusher/243411921ece513be355349adcd1c744} trinity.js}:
      “A fast, minimal JS triple store implementation in ~70 lines of code”
    - {{: https://gist.github.com/jackrusher/6386833} logic-triples.clj}:
      “Transitive properties in a toy triple store using Clojure's core.logic”
    - {{: https://www.w3.org/2001/sw/Europe/events/20031113-storage/positions/rusher.html}
      Triple Store} by Jack Rusher
    - {{: https://web.archive.org/web/20150202180258/http://rhetoricaldevice.com/articles/TripleStore.html}
      Triple Store} by Jack Rusher (Original Version)
*)

type ('s, 'p, 'o) triple = {
  subject : 's;
  predicate : 'p;
  object_ : 'o;
}

let knowledge_base = [
  { subject = `Socrates;    predicate = `is_a;          object_ = `Greek };
  { subject = `Greek;       predicate = `is_a;          object_ = `human_being };
  { subject = `human_being; predicate = `has_property;  object_ = `mortal };
]

let string_of_property = function
  | `Socrates -> "Socrates"
  | `Greek -> "Greek"
  | `human_being -> "human being"
  | `mortal -> "mortal"

let rec collect_predicate_along_axis ~subject ~predicate ~axis kb =
  let facts = kb |> List.filter (fun triple -> triple.subject = subject) in
  let arcs = facts |> List.filter (fun triple -> triple.predicate = axis) in
  List.append
    (facts |> List.filter (fun triple -> triple.predicate = predicate))
    (arcs |> List.concat_map (fun triple ->
      kb |> collect_predicate_along_axis ~subject:triple.object_ ~predicate ~axis))

let get_properties ~subject kb =
  kb
  |> collect_predicate_along_axis ~subject ~predicate:`has_property ~axis:`is_a
  |> List.map (fun triple -> triple.object_)

let () = begin

  Format.printf "Socrates is_a:\n";

  knowledge_base
  |> get_properties ~subject:`Socrates
  |> List.map string_of_property
  |> List.iter (Format.printf "- %s\n");

  Format.printf "\n";

end

(*
Socrates is_a:
- mortal
*)

let knowledge_base = [
  { subject = `Berlin;  predicate = `is_a;    object_ = `city };
  { subject = `Berlin;  predicate = `part_of; object_ = `Germany  };
  { subject = `Germany; predicate = `part_of; object_ = `EU };
  { subject = `EU;      predicate = `part_of; object_ = `Eurasia };
  { subject = `Eurasia; predicate = `part_of; object_ = `Earth };
]

let string_of_property = function
  | `Berlin -> "Berlin"
  | `EU -> "EU"
  | `Earth -> "Earth"
  | `Eurasia -> "Eurasia"
  | `Germany -> "Germany"
  | `Mitteleuropa -> "Mitteleuropa"
  | `NATO -> "NATO"
  | `city-> "city"

let part_of_what ~subject kb =
  kb
  |> collect_predicate_along_axis ~subject ~predicate:`part_of ~axis:`part_of
  |> List.map (fun triple -> triple.object_)

let () = begin

  Format.printf "Berlin is part_of:\n";

  knowledge_base
  |> part_of_what ~subject:`Berlin
  |> List.map string_of_property
  |> List.iter (Format.printf "- %s\n");

  Format.printf "\n";

end

(*
Berlin is part_of:
- Germany
- EU
- Eurasia
- Earth
*)

let knowledge_base = [
  { subject = `Berlin;        predicate = `is_a;      object_ = `city };
  { subject = `Berlin;        predicate = `part_of;   object_ = `Germany };
  { subject = `Germany;       predicate = `part_of;   object_ = `Mitteleuropa };
  { subject = `Germany;       predicate = `member_of; object_ = `EU };
  { subject = `Germany;       predicate = `member_of; object_ = `NATO };
  { subject = `Mitteleuropa;  predicate = `part_of;   object_ = `Eurasia };
  { subject = `Eurasia;       predicate = `part_of;   object_ = `Earth };
]

let () = begin

  Format.printf "Berlin is a member_of:\n";

  knowledge_base
  |> collect_predicate_along_axis ~subject:`Berlin ~predicate:`member_of ~axis:`part_of
  |> List.map (fun triple -> triple.object_)
  |> List.map string_of_property
  |> List.iter (Format.printf "- %s\n");

  Format.printf "\n";
  Format.printf "Berlin is part_of:\n";

  knowledge_base
  |> collect_predicate_along_axis ~subject:`Berlin ~predicate:`part_of ~axis:`part_of
  |> List.map (fun triple -> triple.object_)
  |> List.map string_of_property
  |> List.iter (Format.printf "- %s\n");

  Format.printf "\n";

end

(*
Berlin is a member_of:
- EU
- NATO

Berlin is part_of:
- Germany
- Mitteleuropa
- Eurasia
- Earth
*)
