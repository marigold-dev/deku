#import "errors.mligo" "Errors"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

type address_update = Parameter.Types.t
type storage = Storage.Types.t 

let main (address_update, storage : address_update * storage) = 
  let { key; address; signature } = address_update in 
  let key_hash = Crypto.hash_key key in 
  let () = Storage.Utils.check_signature key address signature in 
  let storage = Big_map.add key_hash address storage in 
  (([] : operation list), storage)