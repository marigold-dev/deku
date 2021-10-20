type t =
  | Implicit(Key_hash.t)
  | Originated({
      contract: Contract_hash.t,
      entrypoint: option(string),
    });

let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
let encoding: Data_encoding.t(t);
let to_yojson: t => Yojson.Safe.t;
let of_yojson: Yojson.Safe.t => result(t, string);
