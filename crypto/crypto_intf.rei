module type S = {
  module Secret: {
    type t;
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
    let hash_key: Key.t => t;
    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };

  module Signature: {
    type t;
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  let sign: (Secret.t, string) => Signature.t;
  let verify: (Key.t, Signature.t, string) => bool;
  let generate: unit => (Secret.t, Key.t);
};
