type coin = Head | Tail

let flip (c : coin) : coin =
  match c with
    Head -> Tail
  | Tail -> Head
