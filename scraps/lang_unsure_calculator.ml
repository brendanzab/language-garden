(** Probabilistic calculator based on Filip Hracek’s Unsure Calculator,
    implemented using probabilistic programming with the approach described by
    Rodrigo Mesquita.

    Resources:

    - Filip Hracek, {{: https://filiph.github.io/unsure/} Unsure Calculator}
    - Rodrigo Mesquita, {{: https://alt-romes.github.io/posts/2025-04-25-unsure-calculator-in-100-lines-of-haskell.html}
      Implementing Unsure Calculator in 100 lines of Haskell}
*)

[@@@warning "-unused-value-declaration"]

(** Monadic computations for building probabilistic distributions.

    Resources:

    - {{: https://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf} Practical Probabilistic Programming with Monads}
    - {{: https://www.youtube.com/watch?v=qZ4O-1VYv4c} The Probability Monad }
    - {{: https://github.com/tweag/monad-bayes} tweag/monad-bayes}
*)
module Dist : sig

  type 'a t

  (** Monadic API *)

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** Construct a normal distribution using the given {i mean} and {i standard
      deviation}. *)
  val normal : mean:float -> std_dev:float -> float t

  (** Randomly sample a value from the distribution. *)
  val sample : Random.State.t -> 'a t -> 'a

  module Notation : sig

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  end

end = struct

  type 'a t =
    | Pure : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Normal : float * float -> float t

  let pure x = Pure x
  let bind d f = Bind (d, f)
  let normal ~mean ~std_dev = Normal (mean, std_dev)

  let map f x = bind x (fun x -> pure (f x))
  let apply f x = bind f (fun f -> map f x)
  let both x y = bind x (fun x -> bind y (fun y -> pure (x, y)))

  (** Transform a pair of {i uniformly} distributed random numbers into a pair
      of {i normally} distributed random numbers.

      - {{: https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform}
        Box–Muller transform} on Wikipedia
  *)
  let box_muller u1 u2 =
    let r = sqrt (-2.0 *. log u1) in
    let t = 2.0 *. Float.pi *. u2 in
    (r *. cos t, r *. sin t)

  let rec sample : type a. Random.State.t -> a t -> a =
    fun s d ->
      match d with
      | Pure x -> x
      | Bind (d, f) ->
          let s' = Random.State.split s in
          sample s (f (sample s' d))
      | Normal (mean, std_dev) ->
          let u1 = Random.State.float s 1.0 in
          let u2 = Random.State.float s 1.0 in
          let (n1, _) = box_muller u1 u2 in
          n1 *. std_dev +. mean

  module Notation = struct

    let ( let* ) = bind
    let ( let+ ) x f = map f x
    let ( and+ ) = both
    let ( <$> ) = map
    let ( <*> ) = apply

  end

end

module Expr : sig

  type t

  val eval : t -> float Dist.t

  val num : float -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val range : t -> t -> t

  module Notation : sig

    val ( ~- ) : t -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( -- ) : t -> t -> t

  end

end = struct

  type t = float Dist.t

  let eval x = x

  open Dist.Notation

  let num x = Dist.pure x
  let neg d = Float.neg <$> d
  let add d1 d2 = Float.add <$> d1 <*> d2
  let sub d1 d2 = Float.sub <$> d1 <*> d2
  let mul d1 d2 = Float.mul <$> d1 <*> d2
  let div d1 d2 = Float.div <$> d1 <*> d2

  let range d1 d2 =
    let* x1, x2 = Dist.both d1 d2 in
    let mean = (x1 +. x2) /. 2.0 in
    let std_dev = (x2 -. x1) /. 4.0 in
    Dist.normal ~mean ~std_dev

  module Notation = struct

    let ( ~- ) = neg
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
    let ( -- ) = range

  end

end

module Histogram : sig

  type t

  val create : num_bins:int -> float array -> t
  val plot : ?num_cols:int -> t -> unit

end = struct

  type t = {
    min_value : float;
    bin_width : float;
    bins : int array;
  }

  let create ~num_bins data =
    if num_bins <= 0 then invalid_arg "Histogram.create";
    if Array.length data <= 0 then invalid_arg "Histogram.create";

    let min_value = Array.fold_left Float.min data.(0) data in
    let max_value = Array.fold_left Float.max data.(0) data in
    let bin_width = (max_value -. min_value) /. Float.of_int num_bins in
    let bins = Array.make num_bins 0 in

    let bin_index x =
      Int.min (Int.of_float ((x -. min_value) /. bin_width)) (num_bins - 1)
    in

    data |> Array.iter begin fun x ->
      let i = bin_index x in
      bins.(i) <- bins.(i) + 1;
    end;

    {
      min_value;
      bin_width;
      bins;
    }

  let plot ?(num_cols = 40) hist =
    let max_count = Array.fold_left Int.max hist.bins.(0) hist.bins in
    let total_count = Array.fold_left Int.add 0 hist.bins in
    let count_scale = Float.of_int num_cols /. Float.of_int max_count in
    let scale_count count = Int.of_float (Float.of_int count *. count_scale) in

    hist.bins |> Array.iteri begin fun i count ->
      let value = hist.min_value +. hist.bin_width *. Float.of_int i in
      let percentage = Float.of_int count /. Float.of_int total_count *. 100.0 in

      Printf.printf "%8.1f (%5.2f%%) | " value percentage;
      for _ = 0 to scale_count count - 1 do
        Printf.printf "▒";
      done;
      Printf.printf "\n";
    end

end

let plot_expr (e : Expr.t) : unit =
  let dist = Expr.eval e in
  let rng = Random.State.make [||] in
  Array.init 25000 (fun _ -> Dist.sample rng dist)
  |> Histogram.create ~num_bins:40
  |> Histogram.plot ~num_cols:40

(** The drake equation from the original blog post *)
let drake : Expr.t =
  let open Expr in
  let open Expr.Notation in

  (num 1.5 -- num 3.0)
    * (num 0.9 -- num 1.0)
    * (num 0.1 -- num 0.4)
    * (num 0.1 -- num 1.0)
    * (num 0.1 -- num 1.0)
    * (num 0.1 -- num 0.2)
    * (num 304.0 -- num 10000.0)

let () =
  plot_expr drake
