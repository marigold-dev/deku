// storage type

type counter = nat;
type threshold = nat;
type authorized_keys = list (key);
type id = string;

type storage = {
  id        : id,
  counter   : counter,
  threshold : threshold,
  auth      : authorized_keys
};

// I/O types

type message = unit => list (operation);

type dummy = (key_hash,signature);

type signatures = list (dummy); /* Waiting to be fixed */

type check_message_pt = {
  counter    : counter,
  message    : message,
  signatures : signatures
};

type return = (list (operation),storage);

type parameter = CheckMessage (check_message_pt);

let check_message = ((param, s): (check_message_pt, storage)) : return =>
{
  let message : message = param.message;
  let s =
    if (param.counter != s.counter) {
      (failwith ("Counters does not match") : storage);
    } else {
    let packed_payload : bytes =
      Bytes.pack ((message, param.counter, s.id, chain_id));
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
      List.fold (aux, param.signatures, (valid, keys));
    if (valid < s.threshold) {
      (failwith ("Not enough signatures passed the check") : storage);
    }
    else {
      {...s,counter : s.counter + 1n};
    };
  };
  (message (unit),s)
};

let main = ((action, store) : (parameter,storage)) : return =>
  switch (action) {
   | CheckMessage (p) => check_message ((p, store))
  }
