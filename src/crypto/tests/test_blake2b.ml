open Deku_crypto

module Make_run
    (Hash : Hash_intf.S) (Expected : sig
      val bits : int
      val hash_tuturu_hex : string
      val hash_mayushii_hex : string
      val hash_invalid_hex : string
      val hash_invalid_small_hex : string
      val hash_invalid_big_hex : string
      val hash_both_tuturu_tuturu_hex : string
      val hash_both_mayushii_mayushii_hex : string
      val hash_both_mayushii_tuturu_hex : string
      val hash_both_tuturu_mayushii_hex : string
    end) : sig
  val run : unit -> unit
end = struct
  open Expected
  open Hash

  let hash' = Alcotest.testable pp equal
  let tuturu = hash "tuturu"
  let mayushii = hash "mayushii"

  let test_equal_tuturu_tuturu () =
    Alcotest.(check' bool)
      ~msg:{|hash "tuturu" = hash "tuturu"|} ~expected:true
      ~actual:(equal tuturu tuturu)

  let test_equal_mayushii_mayushii () =
    Alcotest.(check' bool)
      ~msg:{|hash "mayushii" = hash "mayushii"|} ~expected:true
      ~actual:(equal mayushii mayushii)

  let test_equal_mayushii_tuturu () =
    Alcotest.(check' bool)
      ~msg:{|hash "mayushii" = hash "tuturu"|} ~expected:false
      ~actual:(equal mayushii tuturu)

  let test_equal_tuturu_mayushii () =
    Alcotest.(check' bool)
      ~msg:{|hash "tuturu" = hash "mayushii"|} ~expected:false
      ~actual:(equal tuturu mayushii)

  let test_compare_tuturu_tuturu () =
    Alcotest.(check' int)
      ~msg:{|compare (hash "tuturu") (hash "tuturu")|} ~expected:0
      ~actual:(compare tuturu tuturu)

  let test_compare_mayushii_mayushii () =
    Alcotest.(check' int)
      ~msg:{|compare (hash "mayushii") (hash "mayushii")|} ~expected:0
      ~actual:(compare mayushii mayushii)

  let test_compare_mayushii_tuturu () =
    Alcotest.(check' int)
      ~msg:{|compare (hash "mayushii") (hash "tuturu")|} ~expected:(-1)
      ~actual:(compare mayushii tuturu)

  let test_compare_tuturu_mayushii () =
    Alcotest.(check' int)
      ~msg:{|compare (hash "tuturu") (hash "mayushii")|} ~expected:1
      ~actual:(compare tuturu mayushii)

  let test_of_hex_tuturu () =
    Alcotest.(check' (option hash'))
      ~msg:{|of_hex (to_hex (hash "tuturu"))|} ~expected:(Some tuturu)
      ~actual:(of_hex hash_tuturu_hex)

  let test_of_hex_mayushii () =
    Alcotest.(check' (option hash'))
      ~msg:{|of_hex (to_hex (hash "mayushii"))|} ~expected:(Some mayushii)
      ~actual:(of_hex hash_mayushii_hex)

  let test_of_hex_invalid_hex () =
    Alcotest.(check' (option hash'))
      ~msg:{|of_hex invalid_hex|} ~expected:None
      ~actual:(of_hex hash_invalid_hex)

  let test_of_hex_invalid_small_hex () =
    Alcotest.(check' (option hash'))
      ~msg:{|of_hex invalid_small_hex)|} ~expected:None
      ~actual:(of_hex hash_invalid_small_hex)

  let test_of_hex_invalid_big_hex () =
    Alcotest.(check' (option hash'))
      ~msg:{|of_hex invalid_big_hex)|} ~expected:None
      ~actual:(of_hex hash_invalid_big_hex)

  let test_to_hex_tuturu () =
    Alcotest.(check' string)
      ~msg:{|to_hex (hash "tuturu")|} ~expected:hash_tuturu_hex
      ~actual:(to_hex tuturu)

  let test_to_hex_mayushii () =
    Alcotest.(check' string)
      ~msg:{|to_hex (hash "mayushii")|} ~expected:hash_mayushii_hex
      ~actual:(to_hex mayushii)

  let test_hash_tuturu () =
    Alcotest.(check' string)
      ~msg:{|hash "tuturu"|} ~expected:hash_tuturu_hex
      ~actual:(to_hex (hash "tuturu"))

  let test_hash_mayushii () =
    Alcotest.(check' string)
      ~msg:{|hash "mayushii"|} ~expected:hash_mayushii_hex
      ~actual:(to_hex (hash "mayushii"))

  let test_both_tuturu_tuturu () =
    Alcotest.(check' string)
      ~msg:{|both (hash "tuturu") (hash "tuturu")|}
      ~expected:hash_both_tuturu_tuturu_hex
      ~actual:(to_hex (both tuturu tuturu))

  let test_both_mayushii_mayushii () =
    Alcotest.(check' string)
      ~msg:{|both (hash "mayushii") (hash "mayushii")|}
      ~expected:hash_both_mayushii_mayushii_hex
      ~actual:(to_hex (both mayushii mayushii))

  let test_both_mayushii_tuturu () =
    Alcotest.(check' string)
      ~msg:{|both (hash "mayushii") (hash "tuturu")|}
      ~expected:hash_both_mayushii_tuturu_hex
      ~actual:(to_hex (both mayushii tuturu))

  let test_both_tuturu_mayushii () =
    Alcotest.(check' string)
      ~msg:{|both (hash "tuturu") (hash "mayushii")|}
      ~expected:hash_both_tuturu_mayushii_hex
      ~actual:(to_hex (both tuturu mayushii))

  module Dummy_alg = struct
    type secret = int
    type key = secret
    type signature = { secret : secret; hash : string }

    let sign ?(prefix = "") secret hash = { secret; hash = prefix ^ hash }

    let verify ?(prefix = "") key signature hash =
      let { secret; hash = signature_hash } = signature in
      Int.equal secret key && String.equal signature_hash (prefix ^ hash)
  end

  open With_alg (Dummy_alg)

  let tuturu_sig = sign 3 tuturu
  let mayushii_sig = sign 3 mayushii

  let test_sign_tuturu_secret () =
    Alcotest.(check' int)
      ~msg:{|(sign 3 tuturu).secret|} ~expected:3 ~actual:(sign 3 tuturu).secret

  (* TODO: ensure correct hash
     let test_sign_tuturu_hash () =
        Alcotest.(check' string)
          ~msg:{|(sign 3 tuturu).hash|} ~expected:(to_raw tuturu)
          ~actual:tuturu_sig.hash

      let test_sign_mayushii_hash () =
        Alcotest.(check' string)
          ~msg:{|(sign 3 mayushii).hash|} ~expected:(to_raw mayushii)
          ~actual:mayushii_sig.hash *)

  let test_verify_tuturu_sign () =
    Alcotest.(check' bool)
      ~msg:{|verify 3 (sign 3 tuturu) tuturu|} ~expected:true
      ~actual:(verify 3 tuturu_sig tuturu)

  let test_verify_tuturu_sign_invalid_secret () =
    Alcotest.(check' bool)
      ~msg:{|verify 4 (sign 3 tuturu) tuturu|} ~expected:false
      ~actual:(verify 4 tuturu_sig tuturu)

  let test_verify_tuturu_sign_invalid_sig () =
    Alcotest.(check' bool)
      ~msg:{|verify 3 (sign 3 mayushii) tuturu|} ~expected:false
      ~actual:(verify 3 mayushii_sig tuturu)

  let test_verify_tuturu_sign_invalid_hash () =
    Alcotest.(check' bool)
      ~msg:{|verify 3 (sign 3 tuturu) mayushii|} ~expected:false
      ~actual:(verify 3 tuturu_sig mayushii)

  (* TODO: What_b58 and Map test *)
  let run () =
    let open Alcotest in
    run
      ("BLAKE2b_" ^ string_of_int bits)
      ~and_exit:false
      [
        ( "equal",
          [
            test_case "equal_tuturu_tuturu" `Quick test_equal_tuturu_tuturu;
            test_case "equal_mayushii_mayushii" `Quick
              test_equal_mayushii_mayushii;
            test_case "equal_mayushii_tuturu" `Quick test_equal_mayushii_tuturu;
            test_case "equal_tuturu_mayushii" `Quick test_equal_tuturu_mayushii;
          ] );
        ( "compare",
          [
            test_case "compare_tuturu_tuturu" `Quick test_compare_tuturu_tuturu;
            test_case "compare_mayushii_mayushii" `Quick
              test_compare_mayushii_mayushii;
            test_case "compare_mayushii_tuturu" `Quick
              test_compare_mayushii_tuturu;
            test_case "compare_tuturu_mayushii" `Quick
              test_compare_tuturu_mayushii;
          ] );
        ( "of_hex",
          [
            test_case "of_hex_tuturu" `Quick test_of_hex_tuturu;
            test_case "of_hex_mayushii" `Quick test_of_hex_mayushii;
            test_case "of_hex_invalid_hex" `Quick test_of_hex_invalid_hex;
            test_case "of_hex_invalid_small_hex" `Quick
              test_of_hex_invalid_small_hex;
            test_case "of_hex_invalid_big_hex" `Quick
              test_of_hex_invalid_big_hex;
          ] );
        ( "to_hex",
          [
            test_case "to_hex_tuturu" `Quick test_to_hex_tuturu;
            test_case "to_hex_mayushii" `Quick test_to_hex_mayushii;
          ] );
        ( "hash",
          [
            test_case "hash_tuturu" `Quick test_hash_tuturu;
            test_case "hash_mayushii" `Quick test_hash_mayushii;
          ] );
        ( "both",
          [
            test_case "both_tuturu_tuturu" `Quick test_both_tuturu_tuturu;
            test_case "both_mayushii_mayushii" `Quick
              test_both_mayushii_mayushii;
            test_case "both_mayushii_tuturu" `Quick test_both_mayushii_tuturu;
            test_case "both_tuturu_mayushii" `Quick test_both_tuturu_mayushii;
          ] );
        ( "With_alg",
          [
            test_case "sign_tuturu_secret" `Quick test_sign_tuturu_secret;
            (* test_case "sign_tuturu_hash" `Quick test_sign_tuturu_hash;
               test_case "sign_mayushii_hash" `Quick test_sign_mayushii_hash; *)
            test_case "verify_tuturu_sign" `Quick test_verify_tuturu_sign;
            test_case "verify_tuturu_sign_invalid_secret" `Quick
              test_verify_tuturu_sign_invalid_secret;
            test_case "verify_tuturu_sign_invalid_sig" `Quick
              test_verify_tuturu_sign_invalid_sig;
            test_case "verify_tuturu_sign_invalid_hash" `Quick
              test_verify_tuturu_sign_invalid_hash;
          ] );
      ]
end

module BLAKE2b_256 =
  Make_run
    (BLAKE2b.BLAKE2b_256)
    (struct
      let bits = 256

      let hash_tuturu_hex =
        "b55ce6d1804e12b112c9795f18b81d2ec7ff33047e67a05e0c8603c5e49c3203"

      let hash_mayushii_hex =
        "16f9701ff74483b370665b285bbaa7caf3f4493f4e4344cbb1eb5effd32a06b2"

      let hash_invalid_hex =
        "16f9701ff74483b370665b285bbaa7caf3f4493f4e4344cbb1eb5effd32a06bg"

      let hash_invalid_small_hex =
        "16f9701ff74483b370665b285bbaa7caf3f4493f4e4344cbb1eb5effd32a06"

      let hash_invalid_big_hex =
        "16f9701ff74483b370665b285bbaa7caf3f4493f4e4344cbb1eb5effd32a06b200"

      let hash_both_tuturu_tuturu_hex =
        "f0b0d509bfdc01611b134f81dc5abc7ed0fb6f191ab13d646ecabebf105c9553"

      let hash_both_mayushii_mayushii_hex =
        "23369e300f108366bb9786087eb1cc32b9364b494cb979a8d3c80738d473659e"

      let hash_both_mayushii_tuturu_hex =
        "9e8604d6e9e795594236b2d2b103f61c8909fe024fed7f882addbc311a682ce6"

      let hash_both_tuturu_mayushii_hex =
        "38f78de2740f540c8cafd40ce56a8986b6d9a81d45c883251bc688e255c9e47e"
    end)

module BLAKE2b_160 =
  Make_run
    (BLAKE2b.BLAKE2b_160)
    (struct
      let bits = 160
      let hash_tuturu_hex = "8bee415cad45735e7ea10a32a56a94d7c58bd2ef"
      let hash_mayushii_hex = "26aedcba77c22834884cbb625451893d7cc1b065"
      let hash_invalid_hex = "26aedcba77c22834884cbb625451893d7cc1b06g"
      let hash_invalid_small_hex = "26aedcba77c22834884cbb625451893d7cc1b0"
      let hash_invalid_big_hex = "26aedcba77c22834884cbb625451893d7cc1b06500"

      let hash_both_tuturu_tuturu_hex =
        "cb0c31ad98213fb1908a4d073e0d349cf6591145"

      let hash_both_mayushii_mayushii_hex =
        "abe8c98681f67616cc8a95658fedd83ecd9ce6f7"

      let hash_both_mayushii_tuturu_hex =
        "f164b190ed4e77bb65398d8fd385020f4c43c6ad"

      let hash_both_tuturu_mayushii_hex =
        "046ae718e30ad2d52d070b0e8fc91415a98c5246"
    end)

let run () =
  let () = BLAKE2b_256.run () in
  BLAKE2b_160.run ()
