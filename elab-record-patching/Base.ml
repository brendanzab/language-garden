module List = struct

  include List

  let elem_index a xs =
    let rec go i = function
      | [] -> None
      | x :: xs -> if x = a then Some i else go (i + 1) xs in
    go 0 xs

  let find_dupes xs =
    let rec go dupes = function
      | [] -> List.rev dupes
      | x :: xs when List.mem x xs && not (List.mem x dupes) -> go (x :: dupes) xs
      | _ :: xs -> go dupes xs in
    go [] xs

end
