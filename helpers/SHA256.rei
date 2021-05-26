[@deriving yojson]
type t;
let to_string: t => string;
let compare: (t, t) => int;

let hash: string => t;
let verify: (~hash: t, string) => bool;

module Magic: {
  type hash = t;
  [@deriving yojson]
  type t('a) =
    pri {
      hash,
      data: 'a,
    };

  let hash: 'a => t('a);
  let verify: (~hash: hash, 'a) => result(t('a), string);
};
