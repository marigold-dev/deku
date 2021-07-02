open Helpers;
open Mirage_crypto_ec;

[@deriving (yojson, ord)]
type t('a) = {
  key: Wallet.pub_,
  signature: string,
  data: 'a,
};

[@deriving (yojson, ord)]
type signed('a) = t('a);

let sign = (~key, data) => {
  let message = Cstruct.of_string(Marshal.to_string(data, []));
  let `Hex(signature) = Ed25519.sign(~key, message) |> Hex.of_cstruct;
  {key: Address.of_key(key), signature, data};
};
let verify = (~key, ~signature, data) => {
  let.assert () = (
    "invalid signature",
    Ed25519.verify(
      ~key,
      ~msg=Cstruct.of_string(Marshal.to_string(data, [])),
      `Hex(signature) |> Hex.to_cstruct,
    ),
  );

  Ok({key, signature, data});
};

let of_yojson = (f, json) => {
  open Helpers;
  let.ok {key, signature, data} = of_yojson(f, json);
  verify(~key, ~signature, data);
};

module type S = {
  [@deriving (yojson, ord)]
  type data;
  [@deriving (yojson, ord)]
  type t =
    pri {
      key: Wallet.pub_,
      signature: string,
      data,
    };
  let verify:
    (~key: Wallet.pub_, ~signature: string, data) => result(t, string);
};
module Make =
       (
         F: {
           [@deriving (yojson, ord)]
           type t;
           let verify: (~key: Wallet.pub_, ~signature: string, t) => bool;
         },
       ) => {
  [@deriving (yojson, ord)]
  type data = F.t;
  [@deriving (yojson, ord)]
  type t = {
    key: Wallet.pub_,
    signature: string,
    data,
  };

  let of_yojson = json => {
    let.ok {key, signature, data} = of_yojson(json);
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
