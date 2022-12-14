include List

let rec fold_left_some f state = function
  | [] -> Some state
  | head :: tl -> (
      match f state head with
      | Some state -> fold_left_some f state tl
      | None -> None)

let rec fold_left_ok f state = function
  | [] -> Ok state
  | head :: tl -> (
      match f state head with
      | Ok state -> fold_left_ok f state tl
      | Error error -> Error error)

let chunks_of ~length l = Base.List.chunks_of l ~length
