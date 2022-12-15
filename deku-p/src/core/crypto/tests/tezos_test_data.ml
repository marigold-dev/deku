module type Tezos_data = sig
  val public_keys : string list
  val compared_secret_keys : string list
  val equality_secret_keys : bool
  val equality_public_keys : bool
  val compared_public_keys : string list
  val compared_key_hashes : string list
  val equality_key_hashes : bool
  val to_sign : string
end

module Ed25519_data : Tezos_data = struct
  let public_keys =
    [
      "edpktgopG88M5eE8M6N1ZtHbYzDCXRnGXk9vhNrLnYp9CA6aVyMRXa";
      "edpkvLs7dfWXcdx62iEW5wpYfn8yKPuj446BCRhEbevMMbSSE9G1Yn";
      "edpkvGJ8FdbDSrACkSEzWD1veGeoBQgCTKyX4SVvBc1TBRwcWrbRDQ";
      "edpkvK2woY7vgguhuTZQDVM1hCjbVrEB2dhGVejvgQxHEoGgYqJNuD";
      "edpkutzyeRZkzmcGxQZr7gXTH7Cf7ygDsrn5LSZ2bffHpCeTACB4su";
    ]

  let compared_secret_keys =
    [
      "edsk2kvYWbhbdg6CsgwkZ3svMR76zSJyWUGmpWrRgDRJGJDxZ7aiK3";
      "edsk41Sr6vNDRPenQMNBs11huD26wYMeuJqjFsnC7mNaidVEWuJyh8";
      "edsk44dngys12G6hnRf1VJVTVRcxJWF3nxpPLgKtVJpLzS56eRnYrJ";
      "edsk48oQs2NkiDDNmGfiNnt3NQzL34Cy3tvy9YRCB3RBFXVkkoWnha";
      "edsk4MXvxxHjZKJuW6Rgr1C6tkt6Mwx39o9Tpowco7JjncmSfNb2GF";
    ]

  let equality_secret_keys = true

  let compared_public_keys =
    [
      "edpktgopG88M5eE8M6N1ZtHbYzDCXRnGXk9vhNrLnYp9CA6aVyMRXa";
      "edpkutzyeRZkzmcGxQZr7gXTH7Cf7ygDsrn5LSZ2bffHpCeTACB4su";
      "edpkvGJ8FdbDSrACkSEzWD1veGeoBQgCTKyX4SVvBc1TBRwcWrbRDQ";
      "edpkvK2woY7vgguhuTZQDVM1hCjbVrEB2dhGVejvgQxHEoGgYqJNuD";
      "edpkvLs7dfWXcdx62iEW5wpYfn8yKPuj446BCRhEbevMMbSSE9G1Yn";
    ]

  let equality_public_keys = true

  let compared_key_hashes =
    [
      "tz1LMf9NoTATvLJ7EQjYDzy7XZZ9KHjei5jH";
      "tz1NS9mkwQxD2jgH8kiGVPD33VmLXVVe3wug";
      "tz1UDkGwdCYTyZMG1wwMWMfmbiRtagvP3kXg";
      "tz1aBiQ188pozN9JuSvwUPpkZoajCG6AT7XX";
      "tz1iijagWhWfmG3Cmx7oJ62EkQByPG2foNBU";
    ]

  let equality_key_hashes = true

  let to_sign =
    "1221111113822211222227921529112243121122121222115441211112211232817511621326112702119221122192222173718731151233283141117142231111154312111111323622251114129212"
end
