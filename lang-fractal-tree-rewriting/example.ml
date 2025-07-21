type t =
  | Zero
  | Succ of t

let rules (rules : t -> t) : t -> t =
  function
  | Zero -> Succ Zero
  | Succ n -> Succ (rules n)

let rec step n =
  rules step n

let rec grow (iters : int) (n : t) : t =
  if iters < 0 then n else
    (grow [@tailcall]) (iters - 1) (step n)
