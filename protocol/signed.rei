[@deriving (yojson, ord)]
type t('a) =
  pri {
    key: Address.t,
    signature: string,
    data: 'a,
  };

// TODO: accept a hash function
let sign: (~key: Address.key, 'a) => t('a);
let verify:
  (~key: Address.t, ~signature: string, 'a) => result(t('a), string);
