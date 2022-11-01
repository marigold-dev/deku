module Crypto = struct
  include Deku_crypto.Ed25519

  type id = {
    public_key_hash : Key_hash.t;
    public_key : Key.t;
    secret_key : Secret.t;
  }

  let e1 =
    {
      public_key_hash =
        Option.get @@ Key_hash.of_b58 "tz1NS9mkwQxD2jgH8kiGVPD33VmLXVVe3wug";
      public_key =
        Option.get
        @@ Key.of_b58 "edpktgopG88M5eE8M6N1ZtHbYzDCXRnGXk9vhNrLnYp9CA6aVyMRXa";
      secret_key =
        Option.get
        @@ Secret.of_b58
             "edsk2kvYWbhbdg6CsgwkZ3svMR76zSJyWUGmpWrRgDRJGJDxZ7aiK3";
    }

  let e2 =
    {
      public_key_hash =
        Option.get @@ Key_hash.of_b58 "tz1iijagWhWfmG3Cmx7oJ62EkQByPG2foNBU";
      public_key =
        Option.get
        @@ Key.of_b58 "edpkvLs7dfWXcdx62iEW5wpYfn8yKPuj446BCRhEbevMMbSSE9G1Yn";
      secret_key =
        Option.get
        @@ Secret.of_b58
             "edsk4MXvxxHjZKJuW6Rgr1C6tkt6Mwx39o9Tpowco7JjncmSfNb2GF";
    }

  let e3 =
    {
      public_key_hash =
        Option.get @@ Key_hash.of_b58 "tz1LMf9NoTATvLJ7EQjYDzy7XZZ9KHjei5jH";
      public_key =
        Option.get
        @@ Key.of_b58 "edpkvGJ8FdbDSrACkSEzWD1veGeoBQgCTKyX4SVvBc1TBRwcWrbRDQ";
      secret_key =
        Option.get
        @@ Secret.of_b58
             "edsk48oQs2NkiDDNmGfiNnt3NQzL34Cy3tvy9YRCB3RBFXVkkoWnha";
    }

  let e4 =
    {
      public_key_hash =
        Option.get @@ Key_hash.of_b58 "tz1aBiQ188pozN9JuSvwUPpkZoajCG6AT7XX";
      public_key =
        Option.get
        @@ Key.of_b58 "edpkvK2woY7vgguhuTZQDVM1hCjbVrEB2dhGVejvgQxHEoGgYqJNuD";
      secret_key =
        Option.get
        @@ Secret.of_b58
             "edsk44dngys12G6hnRf1VJVTVRcxJWF3nxpPLgKtVJpLzS56eRnYrJ";
    }

  let e5 =
    {
      public_key_hash =
        Option.get @@ Key_hash.of_b58 "tz1UDkGwdCYTyZMG1wwMWMfmbiRtagvP3kXg";
      public_key =
        Option.get
        @@ Key.of_b58 "edpkutzyeRZkzmcGxQZr7gXTH7Cf7ygDsrn5LSZ2bffHpCeTACB4su";
      secret_key =
        Option.get
        @@ Secret.of_b58
             "edsk41Sr6vNDRPenQMNBs11huD26wYMeuJqjFsnC7mNaidVEWuJyh8";
    }

  let ids = [ e1; e2; e3; e4; e5 ]
end

module Tests = Alg_intf_tests.Test_gen (Crypto) (Tezos_test_data.Ed25519_data)

let run () =
  let open Alcotest in
  let open Tests in
  run "Ed25519 tezos data tests" ~and_exit:false
    [
      ( "Secret key",
        [
          test_case "public keys" `Quick Test_secret_key_data.public_keys;
          test_case "compare" `Quick Test_secret_key_data.compare;
          test_case "equality" `Quick Test_secret_key_data.equality;
        ] );
      ( "Public key",
        [
          test_case "compared" `Quick Test_key_data.compare;
          test_case "equality" `Quick Test_key_data.equality;
        ] );
      ( "Key hash",
        [
          test_case "compare" `Quick Test_key_hash_data.compare;
          test_case "equality" `Quick Test_key_hash_data.equality;
        ] );
      ( "Signatures",
        [
          test_case "to sign" `Quick Test_signature_data.to_sign;
          test_case "signatures" `Quick Test_signature_data.signatures;
          test_case "verified normal signatures" `Quick
            Test_signature_data.verified_normal_signatures;
          test_case "verified after conversion signatures" `Quick
            Test_signature_data.verified_after_conversion;
          test_case "compare" `Quick Test_signature_data.compare;
        ] );
    ]
