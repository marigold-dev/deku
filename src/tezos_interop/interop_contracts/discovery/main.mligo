#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

type uri_update = Parameter.Types.t
type storage = Storage.Types.t

let main (uri_update, storage : uri_update * storage) =
  let { key; uri; nonce; signature } = uri_update in 
  let key_hash = Crypto.hash_key key in 
  let () = Storage.Utils.check_nonce storage key_hash nonce in
  let () = Storage.Utils.check_signature key uri nonce signature in
  let storage = Big_map.add key_hash (nonce, uri) storage in
  (([] : operation list), storage)