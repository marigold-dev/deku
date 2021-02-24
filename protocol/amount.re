[@deriving (ord, yojson)]
type t = int;
let zero = 0;
let (+) = (+);
let (-) = (-);
let of_int = t => {
  // TODO: test this, should amount be non-zero?
  assert(t >= 0);
  t;
};
let to_int = t => t;
