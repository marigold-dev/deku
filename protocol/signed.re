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
[@deriving (yojson, ord)]
type signed('a) = t('a);

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

module Make =
       (
         F: {
           [@deriving yojson]
           type t;
           let verify: (~key: Address.t, ~signature: string, t) => bool;
         },
       ) => {
  type t = {
    key: Address.t,
    signature: string,
    data: F.t,
  };

  let to_yojson = ({key, signature, data}: t) =>
    to_yojson(F.to_yojson, {key, signature, data});
  let of_yojson = json => {
    let.ok {key, signature, data} = of_yojson(F.of_yojson, json);
    let.assert () = ("invalid signature", F.verify(~key, ~signature, data));
    Ok({key, signature, data});
  };
  // TODO: sign()?
  let verify = (~key, ~signature, data) => {
    let.ok {key, signature, data} = verify(~key, ~signature, data);
    let.assert () = ("invalid signature", F.verify(~key, ~signature, data));
    Ok({key, signature, data});
  };
};
