[@deriving (eq, ord)]
// TODO: this should definitely be using Z.t
type t = int;

let zero = 0;
let (+) = (+);
let (-) = (a, b) => {
  let t = a - b;
  assert(t >= 0);
  t;
};
let of_int = t => {
  // TODO: test this, should amount be non-zero?
  assert(t >= 0);
  t;
};
let to_int = t => t;

let of_yojson = json => json |> [%of_yojson: int] |> Result.map(of_int);
let to_yojson = t => `Int(t);
