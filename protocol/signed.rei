[@deriving (yojson, ord)]
type t('a) =
  pri {
    key: Wallet.pub_,
    signature: string,
    data: 'a,
  };
[@deriving (yojson, ord)]
type signed('a) = t('a);
// TODO: accept a hash function
let sign: (~key: Wallet.t, 'a) => t('a);
let verify:
  (~key: Wallet.pub_, ~signature: string, 'a) => result(t('a), string);

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
module Make:
  (
    F: {
      [@deriving (yojson, ord)]
      type t;
      let verify: (~key: Wallet.pub_, ~signature: string, t) => bool;
    },
  ) =>
   S with type data = F.t;
