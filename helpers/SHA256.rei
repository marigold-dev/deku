[@deriving yojson]
type hash;
let hash_to_string: hash => string;
let compare_hash: (hash, hash) => int;

module Magic: {
  [@deriving yojson]
  type t('a) =
    pri {
      hash,
      data: 'a,
    };

  let hash: 'a => t('a);
  let verify: (~hash: hash, 'a) => result(t('a), string);
};
