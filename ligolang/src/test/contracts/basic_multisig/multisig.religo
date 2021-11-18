#include "config.mligo"

// storage type

type counter = c_counter_type
type threshold = c_counter_type
type authorized_keys = list (key);

type storage = {
  id        : string,
  counter   : counter,
  threshold : threshold,
  auth      : authorized_keys
};

// I/O types

type payload = c_payload_type
type dummy = (key_hash,signature);
type signatures = list ((key_hash,signature)); /* Waiting to be fixed */

type parameter = {
  counter    : counter,
  payload    : payload,
  signatures : signatures
};

type return = (list (operation),storage);

let main = ((p, s): (parameter, storage)) : return =>
{
  let payload : payload = p.payload;
  let s =
    if (p.counter != s.counter) {
      (failwith ("Counters does not match") : storage);
    } else {
    let packed_payload : bytes =
      Bytes.pack ((payload, p.counter, s.id, Tezos.chain_id));
      let valid : nat = 0n;
      let keys : authorized_keys = s.auth;
      let aux = ((vk, pkh_sig) :
                 ((nat, authorized_keys), (key_hash, signature)))
                 : (nat, authorized_keys) => {
        let (valid, keys) = vk;
        switch (keys) {
        | [] => vk;
        | [key, ...keys] =>
            if (pkh_sig[0] == Crypto.hash_key (key)) {
              let valid =
                if (Crypto.check (key, pkh_sig[1], packed_payload)) {
                  valid + 1n;
                }
                else { (failwith ("Invalid signature") : nat) };
              (valid, keys);
            }
            else { (valid, keys); };
        };
      };
    let (valid, _keys) =
      List.fold (aux, p.signatures, (valid, keys));
    if (valid < s.threshold) {
      (failwith ("Not enough signatures passed the check") : storage);
    }
    else {
      {...s,counter : s.counter + 1n};
    };
  };
  let contract_opt : option (contract (payload)) = Tezos.get_contract_opt(c_address);
  let op = switch (contract_opt) {
    | Some (c) => [Tezos.transaction(payload, 0tez, c)]
    | None     => (failwith ("Contract not found") : list (operation))
  };
  (op,s)
};
