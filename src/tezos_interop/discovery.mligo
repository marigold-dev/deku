type uri = string
(* TODO: maybe nat *)
type nonce = int
type storage = (key_hash, (nonce * uri)) big_map
type uri_update = {
  key: key;
  uri: uri;
  nonce: nonce;
  signature: signature;
}

let check_nonce (storage: storage) (key_hash: key_hash) (nonce: nonce) =
  match Big_map.find_opt key_hash storage with
  | Some (old_nonce, _) -> 
    if not (nonce > old_nonce) then
      failwith "old nonce"
  | None -> ()

let check_signature
  (key: key)
  (uri: uri)
  (nonce: nonce)
  (signature: signature) =
    let packed_data = Bytes.pack (nonce, uri) in
    if not (Crypto.check key signature packed_data) then
      failwith "invalid signature"

let main (uri_update, storage : uri_update * storage) =
  let key = uri_update.key in
  let key_hash = Crypto.hash_key key in
  let uri = uri_update.uri in
  let nonce = uri_update.nonce in
  let signature = uri_update.signature in

  let () = check_nonce storage key_hash nonce in
  let () = check_signature key uri nonce signature in
  
  let storage = Big_map.add key_hash (nonce, uri) storage in
  (([] : operation list), storage)
