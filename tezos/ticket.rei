type t = {
  ticketer: Address.t,
  data: bytes,
};
let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
