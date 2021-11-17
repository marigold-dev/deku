#include "config.ligo"

// storage type

type counter is c_counter_type
type threshold is c_counter_type
type authorized_keys is list (key)

type storage is
  record [
    id        : string;
    counter   : counter;
    threshold : threshold;
    auth      : authorized_keys
  ]

// I/O types

type payload is c_payload_type
type signatures is list (key_hash * signature)

type parameter is
  record [
    counter    : counter;
    payload    : payload;
    signatures : signatures
  ]

type return is list (operation) * storage


function main (const p : parameter; var s : storage) : return is
block {

  var payload: payload := p.payload;


  if p.counter =/= s.counter then
    failwith ("Counters does not match")
  else {
    const packed_payload : bytes =
      Bytes.pack ((payload, p.counter, s.id, Tezos.chain_id));
    var valid : nat := 0n;

    var pkh_sigs : signatures := p.signatures;
    for key in list s.auth block {
      case pkh_sigs of
        nil -> skip
      | pkh_sig # tl -> block {
          if pkh_sig.0 = Crypto.hash_key (key) then block {
            pkh_sigs := tl;
            if Crypto.check (key, pkh_sig.1, packed_payload)
            then valid := valid + 1n
            else failwith ("Invalid signature")
          }
          else skip
        }
      end
    };

    var _ := pkh_sigs;

    if valid < s.threshold then
      failwith ("Not enough signatures passed the check")
    else s.counter := s.counter + 1n
  };
  const contract_opt : option (contract(payload)) = Tezos.get_contract_opt(c_address);
  var op : list(operation) := nil;
  case contract_opt of
    | Some (c) -> op := list [Tezos.transaction (payload, 0tez, c)]
    | None     -> failwith ("Contract not found")
  end;
} with (op, s)
