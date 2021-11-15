open PP

let%expect_test _ =
  Format.printf "%a" literal (Literal_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]
