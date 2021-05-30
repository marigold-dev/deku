type uri = bytes
type uri_update = {
  key: key;
  uri: uri;
  signature: signature;
}

let check_signature (uri_update: uri_update) =
  let key = uri_update.key in
  let signature = uri_update.signature in
  let uri = uri_update.uri in
  if not (Crypto.check key signature uri) then
    failwith "invalid signature"

let main (uri_update, storage : uri_update * (key, uri) big_map) =
  let () = check_signature uri_update in
  
  let storage = Big_map.add uri_update.key uri_update.uri storage in
  (([] : operation list), storage)