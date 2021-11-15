let x =
  let l = [1; 2; 3;] in
  match l with
  | [_; _; _;] -> 0
  | _          -> 1
