[@deriving yojson]
type t('a) =
  pri {
    hash: string,
    data: 'a,
  };

let hash: 'a => t('a);
let verify: (~hash: string, 'a) => result(t('a), string);
