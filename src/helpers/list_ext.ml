include List
let find_index f l =
  let rec go idx = function
    | [] -> None
    | x :: xs ->
      if f x then
        Some idx
      else
        go (idx + 1) xs in
  go 0 l
let in_order_uniq (type a) compare l =
  let module S = Set.Make (struct
    type t = a
    let compare = compare
  end) in
  let rec go acc seen_set lst =
    match lst with
    | [] -> List.rev acc
    | x :: xs ->
      if S.mem x seen_set then
        go acc seen_set xs
      else
        go (x :: acc) (S.add x seen_set) xs in
  go [] S.empty l
let rec fold_left_ok f state = function
  | [] -> Ok state
  | head :: tl ->
  match f state head with
  | Ok state -> fold_left_ok f state tl
  | Error error -> Error error
