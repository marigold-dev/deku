module type Tezos_data = sig
  val public_keys : string list
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
end
