[@deriving (eq, ord)]
// TODO: this should definitely be using Z.t
type t = int;

let zero = 0;
let (+) = (+);
let (-) = (a, b) => {
  let t = a - b;
  if (t < 0) {
    raise(Invalid_argument("Negative amount"));
  };
  t;
};
let of_int = t => {
  if (t < 0) {
    raise(Invalid_argument("Negative amount"));
  };
  t;
};
let to_int = t => t;

let of_yojson = json => json |> [%of_yojson: int] |> Result.map(of_int);
let to_yojson = t => `Int(t);
