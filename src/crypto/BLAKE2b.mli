module BLAKE2b_160 : Hash_intf.S
module BLAKE2b_256 : Hash_intf.S
include Hash_intf.S with type hash = BLAKE2b_256.hash
