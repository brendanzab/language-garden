module Size = struct

  type t = int

  let zero = 0
  let succ s = s + 1
  let to_int s = s

end

module Index = struct

  type t = int

  let to_level s i = s - i - 1
  let to_int i = i

end

module Level = struct

  type t = int

  let next s = s
  let to_index s l = s - l - 1
  let to_int l = l

end

module Env = struct

  type 'a t = 'a list

  let empty (type a) : a t = []

  let extend (type a) (x : a) (xs : a t) : a t =
    x :: xs

  let lookup (type a) (i : Index.t) (xs : a t) : a =
    List.nth xs i

  let lookup_opt (type a) (i : Index.t) (xs : a t) : a option =
    List.nth_opt xs i

  let size (type a) (xs : a t) : Size.t =
    List.length xs

end
