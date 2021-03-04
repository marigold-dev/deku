open Protocol;

[@deriving (yojson, ord)]
type signature = {
  key: Address.t,
  signature: string,
};

[@deriving (yojson, ord)]
type t('a);

let data: t('a) => 'a;
let signatures: t('a) => list(signature);

let of_signed: Signed.t('a) => t('a);
let sign: (~key: Address.key, 'a) => t('a);
let append_sign: (~key: Address.key, t('a)) => t('a);
let verify_signature: (signature, t('a)) => result(t('a), string);
