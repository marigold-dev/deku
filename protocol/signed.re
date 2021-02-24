open Mirage_crypto;
open Mirage_crypto_pk;
module Rsa_sha256 = Rsa.PSS(Hash.SHA256);

[@deriving (yojson, ord)]
type t('a) = {
  key: Address.t,
  signature: string,
  data: 'a,
};

let sign = (~key, data) => {
  let message = `Message(Cstruct.of_string(Marshal.to_string(data, [])));
  let signature = Rsa_sha256.sign(~key, message) |> Cstruct.to_string;
  {key: Rsa.pub_of_priv(key), signature, data};
};
let verify = (~key, ~signature, data) => {
  let message = `Message(Cstruct.of_string(Marshal.to_string(data, [])));
  Rsa_sha256.verify(~key, ~signature=Cstruct.of_string(signature), message)
    ? Some({key, signature, data}) : None;
};
