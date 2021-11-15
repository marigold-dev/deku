/* WARNING: why the module type here?
   OCaml module boot order doesn't necessarily follow the dep graph,
   t doesn't load modules in sequence if it can skip them because
   you're using module aliases(module X = Y), so this ensures that
   the Random module is loaded before anything else

   TODO: This is a workaround and we should discuss this in the future
    */
// TODO: the following two modules probably shouldn't be exposed here
module Base58: (module type of Base58);
module Encoding_helpers: (module type of Encoding_helpers);
module Random: (module type of Random);
module Secp256k1: (module type of Secp256k1);
module Ed25519: (module type of Ed25519);
module Incremental_patricia: (module type of Incremental_patricia);
module BLAKE2B: (module type of BLAKE2B);
module BLAKE2B_20: (module type of BLAKE2B_20);
module Secret: (module type of Secret);
module Key: (module type of Key);
module Key_hash: (module type of Key_hash);
module Signature: (module type of Signature);
