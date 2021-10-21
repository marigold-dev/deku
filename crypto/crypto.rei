module Base58 = Base58;
module Ed25519 = Ed25519;
module Incremental_patricia = Incremental_patricia;
module BLAKE2B = BLAKE2B;
module BLAKE2B_20 = BLAKE2B_20;
module Random = Random;

let blake2b_20_encoding: Data_encoding.t(BLAKE2B_20.t);
