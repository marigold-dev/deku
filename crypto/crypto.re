Random.self_init();
Mirage_crypto_rng_unix.initialize();

let blake2b_20_encoding = Blake2b_helpers.blake2b_20_encoding;
module Base58 = Base58;
module Ed25519 = Ed25519;
