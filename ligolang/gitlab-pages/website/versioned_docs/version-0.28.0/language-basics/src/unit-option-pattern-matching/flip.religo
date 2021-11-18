type coin = | Head | Tail;

let flip = (c : coin) : coin =>
  switch (c) {
  | Head => Tail
  | Tail => Head
  };
