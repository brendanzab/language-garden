type t =
  | User of string
  | Machine of string

let pp fmt name =
  match name with
  | User name -> Format.fprintf fmt "%s" name
  | Machine name -> Format.fprintf fmt "$%s" name
