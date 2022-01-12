[@deriving (eq, ord, yojson)]
type t;

let zero: t;
let (+): (t, t) => t;
let (-): (t, t) => t;

let of_int: int => t;
let to_int: t => int;
