open Cli_expect

let%expect_test _ =
  run_ligo_good ["run" ; "interpret" ; "(\"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7\":signature)" ; "--syntax"; "pascaligo"] ;
  [%expect {|
    Signature edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7 |}]

let%expect_test _ =
  run_ligo_bad ["run" ; "interpret" ; "(\"thisisnotasignature\":signature)" ; "--syntax";"pascaligo"] ;
  [%expect {|
     Ill-formed literal "Signature thisisnotasignature".
    In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
    In the case of a key_hash, signature, or key a Base58 encoded hash is expected. |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "(\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\":key)" ; "--syntax";"pascaligo"] ;
  [%expect {|
    key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav |}]

let%expect_test _ =
  run_ligo_bad ["run"; "interpret" ; "(\"thisisnotapublickey\":key)" ; "--syntax";"pascaligo"] ;
  [%expect {|
     Ill-formed literal "key thisisnotapublickey".
    In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
    In the case of a key_hash, signature, or key a Base58 encoded hash is expected. |}]
