#import "errors.mligo" "Errors"

module Types = struct
  type t = (key_hash, address) big_map
end

module Utils = struct 
  let check_signature (key : key) (address : address) (signature : signature) =
    let packed_address = Bytes.pack address in
    assert_with_error (Crypto.check key signature packed_address) Errors.invalid_signature
end