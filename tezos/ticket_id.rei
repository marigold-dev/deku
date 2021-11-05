[@deriving (eq, ord, yojson)]
type t = {
  ticketer: Address.t,
  data: bytes,
};
let to_string: t => string;
let of_string: string => option(t);
