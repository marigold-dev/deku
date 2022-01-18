module type S = {
  module Secret: {
    type t;
    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  module Key: {
    type t;

    let of_secret: Secret.t => t;

    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  module Key_hash: {
    type t;

    let of_key: Key.t => t;

    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };

  module Signature: {
    type t;
    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  let sign: (Secret.t, BLAKE2B.t) => Signature.t;
  let verify: (Key.t, Signature.t, BLAKE2B.t) => bool;
  let generate: unit => (Secret.t, Key.t);
};
