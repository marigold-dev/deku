#include "config.mligo"

// storage type

type counter = c_counter_type
type threshold = c_counter_type
type authorized_keys = key list

type storage = {
  id        : string;
  counter   : counter;
  threshold : threshold;
  auth      : authorized_keys
}

// I/O types

type payload = c_payload_type
type signatures = (key_hash * signature) list

type parameter = {
  counter    : counter;
  payload    : payload;
  signatures : signatures
}

type return = operation list * storage


let main (p, s : parameter * storage) : return =
  let payload : payload = p.payload in
  let s =
    if p.counter <> s.counter then
      (failwith "Counters does not match" : storage)
    else
      let packed_payload : bytes =
        Bytes.pack (payload, p.counter, s.id, Tezos.chain_id) in
      let valid : nat = 0n in
      let keys : authorized_keys = s.auth in
      let aux =
        fun (vk, pkh_sig: (nat * authorized_keys)*(key_hash * signature)) ->
          let valid, keys = vk in
            match keys with
            | [] -> vk
            | key::keys ->
                if pkh_sig.0 = Crypto.hash_key key
                then
                  let valid =
                    if Crypto.check key pkh_sig.1 packed_payload
                    then valid + 1n
                    else (failwith "Invalid signature" : nat)
                  in valid, keys
                else valid, keys in
      let valid, _keys  =
        List.fold aux p.signatures (valid, keys) in
      if valid < s.threshold then
        (failwith ("Not enough signatures passed the check") : storage)
      else {s with counter = s.counter + 1n}
    in
    let contract_opt : payload contract option = Tezos.get_contract_opt(c_address) in
    let op = match contract_opt with
        Some (c) -> [Tezos.transaction payload 0tez c]
      | None     -> (failwith ("Contract not found") : operation list)
    in
    op, s
