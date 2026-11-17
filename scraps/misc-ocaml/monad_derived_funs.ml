(** Implementing monads with derived functions, making use of OCaml’s
    module-dependent functions.
*)

module Monad = struct

  module type S = sig

    type 'a t

    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val app : ('a -> 'b) t -> 'a t -> 'b t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val join : 'a t t -> 'a t

  end


  (** Minimal signatures *)

  module type With_bind = sig

    type 'a t

    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t

  end

  module type With_map_and_join = sig

    type 'a t

    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val join : 'a t t -> 'a t

  end

  module _ (M : S) : With_bind = M
  module _ (M : S) : With_map_and_join = M


  (** Derived functions *)

  let map (module M : With_bind) (f : 'a -> 'b) (m : 'a M.t) : 'b M.t =
    M.bind m (fun x -> M.pure (f x))

  let app (module M : With_bind) (f : ('a -> 'b) M.t) (m : 'a M.t) : 'b M.t =
    M.bind f (fun f -> map (module M) f m)

  let join (module M : With_bind) (m : 'a M.t M.t) : 'a M.t =
    M.bind m Fun.id

  (* It's annoying that we need to add a ['] here. How do we handle when
     multiple default functions can be implemented for different signatures?. *)
  let app' (module M : With_map_and_join) (f : ('a -> 'b) M.t) (m : 'a M.t) : 'b M.t =
    M.join (f |> M.map (fun f -> m |> M.map f))

  let bind (module M : With_map_and_join) (m : 'a M.t) (f : 'a -> 'b M.t) : 'b M.t =
    M.join (M.map f m)


  (** Standard library implementations *)

  module Option : S with type 'a t = 'a option = struct

    module Base = struct

      type 'a t = 'a option

      let pure = Option.some
      let bind = Option.bind

    end

    include Base

    let map f m = map (module Base) f m
    let app f m = app (module Base) f m
    let join m = join (module Base) m

  end

  module List : S with type 'a t = 'a list = struct

    module Base = struct

      type 'a t = 'a list

      let pure = List.singleton
      let bind xs f = List.concat_map f xs

    end

    include Base

    let map f m = map (module Base) f m
    let app f m = app (module Base) f m
    let join m = join (module Base) m

  end

  module Seq : S with type 'a t = 'a Seq.t = struct

    module Base = struct

      type 'a t = 'a Seq.t

      let pure = Seq.singleton
      let bind xs f = Seq.concat_map f xs

    end

    include Base

    let map f m = map (module Base) f m
    let app f m = app (module Base) f m
    let join m = join (module Base) m

  end

end
