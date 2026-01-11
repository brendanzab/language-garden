(** Graph library based on a small set of construction primitives.

    - {{: https://doi.org/10.1145/3122955.3122956} Algebraic graphs with class (functional pearl)}
    - {{: https://discourse.haskell.org/t/functional-representations-of-graphs/13016} Functional representations of Graphs}

    Implementation status:

    - [x] 2.2 Type Class
    - [x] 3.1 Axiomatic Characterisation
    - [x] 3.2 Partial Order on Graphs
    - [x] 4.1 Binary Relation
    - [x] 4.2 Deep Embedding
    - [ ] 4.3 Undirected Graphs
    - [ ] 4.4 Reflexive Graphs
    - [ ] 4.5 Transitive Graphs
    - [ ] 4.6 Preorders and Equivalence Relations
    - [ ] 4.7 Hypergraphs
    - [x] 5.1 Standard Families of Graphs
    - [x] 5.2 Graph Transpose
    - [ ] 5.3 Graph Functor
    - [ ] 5.4 Graph Monad
    - [ ] 5.5 Beyond Homomorphisms
    - [ ] 5.6 De Bruijn Graphs
*)

module Compare = struct

  module type S = sig

    type t

    val compare : t -> t -> int

  end

end

module Monoid = struct


  module type S = sig

    type t

    val combine : t -> t -> t
    val empty : t

    (** The following laws must hold:

        - [combine (combine x y) z] = [combine x (combine y z)]
        - [combine empty x] = [x] = [combine x empty]
    *)

  end

end

module Tree = struct

  type 'a t = {
    root : 'a;
    forest : 'a forest;
  }

  and 'a forest = 'a t list

end

module Graph = struct

  (** Core algebra for constructing graphs *)
  module type Core = sig

    type t
    type vertex

    val empty : t
    val vertex : vertex -> t
    val overlay : t -> t -> t
    val connect : t -> t -> t

    (** The following laws must hold:

        - [overlay (overlay x y) z] = [overlay x (overlay y z)]
        - [overlay x y] = [overlay y x]
        - [overlay empty x] = [x] = [overlay x empty]
        - [connect (connect x y) z] = [connect x (connect y z)]
        - [connect empty x] = [x] = [connect x empty]
        - [connect x (overlay y z)] = [overlay (connect x y) (connect x z)]
        - [connect (overlay x y) z] = [overlay (connect x z) (connect y z)]
        - [connect (connect x y) z] = [overlay (overlay (connect x y) (connect x z)) (connect y z)]
    *)

  end

  module type S = sig

    include Core

    (** [(t, overlay, empty)] is an idempotent commutative monoid of *)
    module Overlay_monoid : Monoid.S with type t = t

    (** [(t, connect, empty)] is a monoid *)
    module Connect_monoid : Monoid.S with type t = t

    val edge : vertex -> vertex -> t
    val vertices : vertex list -> t
    val clique : vertex list -> t
    val edges : (vertex * vertex) list -> t
    val graph : vertex list -> (vertex * vertex) list -> t
    val is_subgraph_of : (t -> t -> bool) -> t -> t -> bool

    val path : vertex list -> t
    val circuit : vertex list -> t
    val star : vertex -> vertex list -> t
    val tree : vertex Tree.t -> t
    val forest : vertex Tree.forest -> t

    module Notation : sig

      val v : vertex -> t

      val ( ++ ) : t -> t -> t
      val ( ** ) : t -> t -> t

    end

  end

  module Make (G : Core) : S
    with type t = G.t
    with type vertex = G.vertex
  = struct

    include G

    module Overlay_monoid = struct

      type nonrec t = t

      let combine = overlay
      let empty = empty

    end

    module Connect_monoid = struct

      type nonrec t = t

      let combine = connect
      let empty = empty

    end

    let edge : vertex -> vertex -> t =
      fun x y -> connect (vertex x) (vertex y)

    let vertices : vertex list -> t =
      ListLabels.fold_left ~init:empty ~f:(fun acc x -> overlay acc (vertex x))

    let clique : vertex list -> t =
      ListLabels.fold_left ~init:empty ~f:(fun acc x -> connect acc (vertex x))

    let edges : (vertex * vertex) list -> t =
      ListLabels.fold_left ~init:empty ~f:(fun acc (x, y) -> overlay acc (edge x y))

    let graph : vertex list -> (vertex * vertex) list -> t =
      fun vs es ->
        overlay (vertices vs) (edges es)

    let is_subgraph_of : (t -> t -> bool) -> t -> t -> bool =
      fun equal x y ->
        equal (overlay x y) y

    let nonempty_path x xs =
      ListLabels.fold_left xs ~init:(x, empty)
        ~f:(fun (x, acc) y -> y, overlay acc (edge x y))

    let path : vertex list -> t = function
      | [] -> empty
      | [x] -> vertex x
      | x :: xs -> snd (nonempty_path x xs)

    let circuit : vertex list -> t = function
      | [] -> empty
      | [x] -> edge x x
      | x :: xs ->
          let y, acc = nonempty_path x xs in
          overlay acc (edge y x)

    let star : vertex -> vertex list -> t =
      fun x ys ->
        connect (vertex x) (vertices ys)

    let rec tree : vertex Tree.t -> t =
      fun t ->
        overlay (star t.root (t.forest |> List.map Tree.(fun t -> t.root))) (forest t.forest)

    and forest : vertex Tree.forest -> t =
      fun ts ->
        ts |> ListLabels.fold_left ~init:empty ~f:(fun acc t -> overlay acc (tree t))

    module Notation = struct

      let v = vertex

      let ( ++ ) = overlay
      let ( ** ) = connect

    end

  end

  module Relation (Vertex : Compare.S) : sig

    module Vertex_set : Set.S
    module Edge_set : Set.S

    type t = {
      domain : Vertex_set.t;
      relation : Edge_set.t;
    }

    type vertex = Vertex.t

    include S
      with type t := t
      with type vertex := vertex

    val equal : t -> t -> bool
    val edge_list : t -> (vertex * vertex) list
    val edge_seq : t -> (vertex * vertex) Seq.t [@@warning "-unused-value-declaration"]

  end = struct

    module T = struct

      module Edge = struct

        type t = Vertex.t * Vertex.t
        let compare = compare

      end

      module Vertex_set = Set.Make (Vertex)
      module Edge_set = Set.Make (Edge)

      type t = {
        domain : Vertex_set.t;
        relation : Edge_set.t;
      }

      type vertex = Vertex.t

      let empty = {
        domain = Vertex_set.empty;
        relation = Edge_set.empty;
      }

      let vertex x = {
        domain = Vertex_set.singleton x;
        relation = Edge_set.empty;
      }

      let overlay x y = {
        domain = Vertex_set.union x.domain y.domain;
        relation = Edge_set.union x.relation y.relation;
      }

      let connect x y = {
        domain = Vertex_set.union x.domain y.domain;
        relation =
          Edge_set.(union x.relation y.relation)
            |> Edge_set.add_seq (Seq.product (Vertex_set.to_seq x.domain) (Vertex_set.to_seq y.domain));
      }

    end

    (* NOTE: Will be cleaner with [include functor] *)
    include Make (T)
    include T

    let equal : t -> t -> bool =
      fun x y ->
        Vertex_set.equal x.domain y.domain
          && Edge_set.equal x.relation y.relation

    let edge_list : t -> (vertex * vertex) list =
      fun x -> Edge_set.to_list x.relation

    let edge_seq : t -> (vertex * vertex) Seq.t =
      fun x -> Edge_set.to_seq x.relation

  end

  module Data (Vertex : sig type t end) : sig

    type t =
      | Empty
      | Vertex of Vertex.t
      | Overlay of t * t
      | Connect of t * t

    type vertex = Vertex.t

    include S
      with type t := t
      with type vertex := vertex

    val fold : (module Core with type t = 'g and type vertex = vertex) -> t -> 'g [@@warning "-unused-value-declaration"]
    val equal : (module Compare.S with type t = vertex) -> t -> t -> bool [@@warning "-unused-value-declaration"]

  end = struct

    module T = struct

      type t =
        | Empty
        | Vertex of Vertex.t
        | Overlay of t * t
        | Connect of t * t

      type vertex = Vertex.t

      let empty = Empty
      let vertex x = Vertex x
      let overlay x y = Overlay (x, y)
      let connect x y = Connect (x, y)

    end

    (* NOTE: Will be cleaner with [include functor] *)
    include Make (T)
    include T

    let fold (type g) (module G : Core with type t = g and type vertex = vertex) : t -> g =
      let rec fold = function
        | Empty -> G.empty
        | Vertex x -> G.vertex x
        | Overlay (x, y) -> G.overlay (fold x) (fold y)
        | Connect (x, y) -> G.connect (fold x) (fold y)
      in
      fold

    let equal (module V : Compare.S with type t = vertex) : t -> t -> bool =
      let module G = Relation (V) in
      fun x y ->
        G.equal (fold (module G) x) (fold (module G) y)

  end

  module Transpose (G : S) : sig

    type t

    val transpose : t -> G.t

    include S
      with type t := t
      with type vertex = G.vertex

  end = struct

    module T = struct

        type t = G.t

        type vertex = G.vertex

        let transpose x = x

        let empty = G.empty
        let vertex x = G.vertex x
        let overlay x y = G.overlay x y
        let connect x y = G.connect y x

    end

    (* NOTE: Will be cleaner with [include functor] *)
    include Make (T)
    include T

  end

end

let () = begin

  Printf.printf "Running tests in %s ..." __FILE__;

  Printexc.record_backtrace true;

  let module Char_graph = Graph.Relation (Char) in
  let module Int_graph = Graph.Relation (Int) in

  (* Section 3 *)

  begin

    let open Int_graph in
    let open Int_graph.Notation in

    (* Distributivity *)
    assert (equal (v 1 ** (v 2 ++ v 3)) (v 1 ** v 2 ++ v 1 ** v 3));

    (* Decomposition *)
    assert (equal (v 1 ** v 2 ** v 3) (v 1 ** v 2 ++ v 1 ** v 3 ++ v 2 ** v 3));

  end;


  (* Section 4.1 *)

  begin

    let open Int_graph in
    let open Int_graph.Notation in

    assert (equal (v 1 ** (v 2 ++ v 3) ++ (v 2 ** v 3)) (clique [1; 2; 3]));
    assert (not (equal (v 1 ** v 2) (v 2 ** v 1)));

  end;

  (* Section 5.1 *)

  begin

    let p4 = Char_graph.path ['a'; 'b'; 'c'; 'd'] in
    assert (Char_graph.edge_list p4 = ['a', 'b'; 'b', 'c'; 'c', 'd']);

    let p4 = Char_graph.circuit ['a'; 'b'; 'c'; 'd'] in
    assert (Char_graph.edge_list p4 = ['a', 'b'; 'b', 'c'; 'c', 'd'; 'd', 'a']);

    let pentagon_verts = Seq.(ints 1 |> take 5) |> List.of_seq in
    let pentagon = Int_graph.circuit pentagon_verts in
    assert (Int_graph.is_subgraph_of Int_graph.equal (Int_graph.path pentagon_verts) pentagon);

  end;

  (* Section 5.2 *)

  begin

    let g = Int_graph.(Notation.(v 1 ** (v 2 ++ v 3) ** v 4)) in
    assert (Int_graph.edge_list g = [1, 2; 1, 3; 1, 4; 2, 4; 3, 4]);

    let module Int_graph_t = Graph.Transpose (Int_graph) in
    let g = Int_graph_t.Notation.(v 1 ** (v 2 ++ v 3) ** v 4) |> Int_graph_t.transpose in
    assert (Int_graph.edge_list g = [2, 1; 3, 1; 4, 1; 4, 2; 4, 3]);

  end;

  Printf.printf " ok!\n";

end
