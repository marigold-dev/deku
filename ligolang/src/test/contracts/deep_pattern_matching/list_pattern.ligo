const a =
  case list [1] of 
  | nil -> 1
  | list [ a; b ; c] -> 2
  | _ -> 3
  end