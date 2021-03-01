open Helpers;
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
  let.assert () = (
    "invalid signature",
    Rsa_sha256.verify(
      ~key,
      ~signature=Cstruct.of_string(signature),
      message,
    ),
  );

  Ok({key, signature, data});
};

let of_yojson = (f, json) => {
  open Helpers;
  let.ok {key, signature, data} = of_yojson(f, json);
  verify(~key, ~signature, data);
};
