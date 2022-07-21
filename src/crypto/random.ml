let () =
  Stdlib.Random.self_init ();
  Mirage_crypto_rng_unix.initialize ()

let generate bits = Mirage_crypto_rng.generate bits
let int32 = Stdlib.Random.int32
