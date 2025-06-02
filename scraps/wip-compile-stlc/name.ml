type t =
  | User of string
  | Machine of string

let pp ppf name =
  match name with
  | User name -> Format.fprintf ppf "%s" name
  | Machine name -> Format.fprintf ppf "$%s" name
