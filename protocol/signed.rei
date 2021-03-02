[@deriving (yojson, ord)]
type t('a) =
  pri {
    key: Address.t,
    signature: string,
    data: 'a,
  };
[@deriving (yojson, ord)]
type signed('a) = t('a);
// TODO: accept a hash function
let sign: (~key: Address.key, 'a) => t('a);
let verify:
  (~key: Address.t, ~signature: string, 'a) => result(t('a), string);

module Make:
  (
    F: {
      [@deriving yojson]
      type t;
      let verify: (~key: Address.t, ~signature: string, t) => bool;
    },
  ) =>
   {
    // TODO: is this pri here meaningful?
    [@deriving yojson]
    type t =
      pri {
        key: Address.t,
        signature: string,
        data: F.t,
      };
    let verify:
      (~key: Address.t, ~signature: string, F.t) => result(t, string);
  };
