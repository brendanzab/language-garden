module List = struct

  include List

  let find_dupes xs =
    let rec go dupes = function
      | [] -> List.rev dupes
      | x :: xs when List.mem x xs && not (List.mem x dupes) -> go (x :: dupes) xs
      | _ :: xs -> go dupes xs in
    go [] xs

end
