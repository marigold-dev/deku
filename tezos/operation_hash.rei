open Helpers;
type t = BLAKE2B.t;
let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
