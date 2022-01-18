[@deriving (ord, yojson)]
type t;
let zero: t;
let (+): (t, t) => t;
let (-): (t, t) => t;
// TODO: is this okay to be public?
let of_int: int => t;
// TODO: is this okay to be public?
let to_int: t => int;
