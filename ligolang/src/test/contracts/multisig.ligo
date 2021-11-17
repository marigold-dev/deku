// storage type

type counter is nat
type threshold is nat
type authorized_keys is list (key)
type id is string

type storage is
  record [
    id        : id;
    counter   : counter;
    threshold : threshold;
    auth      : authorized_keys
  ]

// I/O types

type message is unit -> list (operation)

type signatures is list (key_hash * signature)

type check_message_pt is
  record [
    counter    : counter;
    message    : message;
    signatures : signatures
  ]

type return is list (operation) * storage

type parameter is CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        var s : storage) : return is block {
  var message : message := param.message;

  if param.counter =/= s.counter then
    failwith ("Counters does not match")
  else {
    const packed_payload : bytes =
      Bytes.pack ((message, param.counter, s.id, Tezos.chain_id));
    var valid : nat := 0n;

    var keys : authorized_keys := s.auth;
    for pkh_sig in list param.signatures block {
      case keys of
        nil -> skip
      | key # tl -> block {
          keys := tl;
          if pkh_sig.0 = Crypto.hash_key (key) then
            if Crypto.check (key, pkh_sig.1, packed_payload)
            then valid := valid + 1n
            else failwith ("Invalid signature")
          else skip
        }
      end
    };

    var _ := keys;

    if valid < s.threshold then
      failwith ("Not enough signatures passed the check")
    else s.counter := s.counter + 1n
  }
} with (message (unit), s)

function main (const param : parameter; const s : storage) : return is
  case param of CheckMessage (p) -> check_message (p,s) end
