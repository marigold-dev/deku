// storage type

type counter = nat
type threshold = nat
type authorized_keys = key list
type id = string

type storage = {
  id        : id;
  counter   : counter;
  threshold : threshold;
  auth      : authorized_keys
}

// I/O types

type message = unit -> operation list

type signatures = (key_hash * signature) list

type check_message_pt = {
  counter    : counter;
  message    : message;
  signatures : signatures
}

type return = operation list * storage

type parameter = CheckMessage of check_message_pt

let check_message (param, s : check_message_pt * storage) : return =
  let message : message = param.message in
  let s =
    if param.counter <> s.counter then
      (failwith "Counters does not match" : storage)
    else
      let packed_payload : bytes =
        Bytes.pack (message, param.counter, s.id, chain_id) in
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
        List.fold aux param.signatures (valid, keys) in
      if valid < s.threshold then
        (failwith ("Not enough signatures passed the check") : storage)
      else {s with counter = s.counter + 1n}
    in message unit, s

let main (action, store : parameter * storage) : return =
  match action with
    CheckMessage (p) -> check_message (p, store)
