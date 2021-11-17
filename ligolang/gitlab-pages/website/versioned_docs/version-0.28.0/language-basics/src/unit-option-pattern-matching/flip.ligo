type coin is Head | Tail

function flip (const c : coin) : coin is
  case c of
    Head -> Tail
  | Tail -> Head
  end
