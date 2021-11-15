let k (x : int) (_y : int) = x

let k2 (x : int) (_ : int) = x

let m = match (Some 4) with
  | Some _x -> 1
  | None -> 0

let m2 = match (Some 4) with
  | Some _ -> 1
  | None -> 0
