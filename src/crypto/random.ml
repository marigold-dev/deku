Stdlib.Random.self_init ()
let () = Mirage_crypto_rng_unix.initialize ()
open Mirage_crypto_rng
let generate bits = generate bits
let int32 = Stdlib.Random.int32
