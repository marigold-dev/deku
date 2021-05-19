[@deriving yojson]
type hash;

let compare_hash: (hash, hash) => int;

[@deriving yojson]
type t('a) =
  pri {
    hash,
    data: 'a,
  };

let hash: 'a => t('a);
let verify: (~hash: hash, 'a) => result(t('a), string);
