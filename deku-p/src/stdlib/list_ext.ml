include List

let rec fold_left_ok f state = function
  | [] -> Ok state
  | head :: tl -> (
      match f state head with
      | Ok state -> fold_left_ok f state tl
      | Error error -> Error error)
