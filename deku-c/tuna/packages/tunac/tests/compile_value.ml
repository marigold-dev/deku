let value = Alcotest.of_pp Tunac.Values.V.pp

let error = Alcotest.of_pp (fun _fmt _t -> ())

let integers () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok (Tunac.Values.V.Int (Z.of_int 42)))
    (Tunac.Compiler.compile_value "42")

let booleans () =
  Alcotest.(check @@ result value error)
    "Same value" (Ok (Tunac.Values.Bool 0))
    (Tunac.Compiler.compile_value "False");
  Alcotest.(check @@ result value error)
    "Same value" (Ok (Tunac.Values.Bool 1))
    (Tunac.Compiler.compile_value "True")

let bytes_ () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok (Tunac.Values.V.Bytes (Bytes.of_string "ABC")))
    (Tunac.Compiler.compile_value "0x414243");
  Alcotest.(check @@ result value error)
    "Same value" (Ok (Tunac.Values.Bytes Bytes.empty))
    (Tunac.Compiler.compile_value "0x")

let strings () =
  Alcotest.(check @@ result value error)
    "Same value" (Ok (Tunac.Values.String "Alcotest"))
    (Tunac.Compiler.compile_value "\"Alcotest\"")

let unit_ () =
  Alcotest.(check @@ result value error)
    "Same value" (Ok Tunac.Values.Unit)
    (Tunac.Compiler.compile_value "Unit")

let pairs () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.(Pair (Bool 1, Int (Z.of_int 42))))
    (Tunac.Compiler.compile_value "(Pair True 42)")

let unions () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.(Union (Left (Int (Z.of_int 13)))))
    (Tunac.Compiler.compile_value "(Left 13)");
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.(Union (Right (Int (Z.of_int 45)))))
    (Tunac.Compiler.compile_value "(Right 45)")

let optionals () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.(Option None))
    (Tunac.Compiler.compile_value "None");
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.V.(Option (Some (String "Hello world"))))
    (Tunac.Compiler.compile_value "(Some \"Hello world\")")

let lists () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok Tunac.Values.(List []))
    (Tunac.Compiler.compile_value "{ }");
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok
       Tunac.Values.(
         List [ Int (Z.of_int 0); Int (Z.of_int 1); Int (Z.of_int 3) ]))
    (Tunac.Compiler.compile_value "{ 0; 1; 3 }")

let maps () =
  Alcotest.(check @@ result value error)
    "Same value"
    (Ok
       Tunac.Values.(
         Map
           (Map.of_seq
              (List.to_seq
                 [ (Int (Z.of_int 0), String "zero")
                 ; (Int (Z.of_int 1), String "one")
                 ; (Int (Z.of_int 3), String "three")
                 ]))))
    (Tunac.Compiler.compile_value
       "{ Elt 0 \"zero\"; Elt 1 \"one\" ; Elt 3 \"three\" }")

let fa12_storage () =
  let unparsed_value =
    {|
      (Pair
        { Elt "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM"
              (Pair { Elt "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU" 500 } 10000)
        ; Elt "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
              (Pair { Elt "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" 1000 } 50000)
        ; Elt "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK"
              (Pair EMPTY_MAP 1000) }
        4000)
    |}
  in
  let expected =
    Tunac.Values.(
      Pair
        ( Map
            (Map.of_seq
               (List.to_seq
                  [ ( String "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM"
                    , Pair
                        ( Map
                            (Map.of_seq
                               (List.to_seq
                                  [ ( String
                                        "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
                                    , Int (Z.of_int 500) )
                                  ]))
                        , Int (Z.of_int 10000) ) )
                  ; ( String "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
                    , Pair
                        ( Map
                            (Map.of_seq
                               (List.to_seq
                                  [ ( String
                                        "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK"
                                    , Int (Z.of_int 1000) )
                                  ]))
                        , Int (Z.of_int 50000) ) )
                  ; ( String "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK"
                    , Pair (Map Map.empty, Int (Z.of_int 1000)) )
                  ]))
        , Int (Z.of_int 4000) ))
  in
  Alcotest.(check @@ result value error)
    "Same value" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value)

let fa12_entrypoints () =
  let unparsed_value =
    {| (Left (Left (Left (Pair "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU" 1000)))) |}
  in
  let expected =
    Tunac.Values.(
      Union
        (Left
           (Union
              (Left
                 (Union
                    (Left
                       (Pair
                          ( String "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
                          , Int (Z.of_int 1000) ))))))))
  in
  Alcotest.(check @@ result value error)
    "%%approve" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value);

  let unparsed_value =
    {| ( Left
        ( Left
          ( Right
            ( Pair
              ( Pair "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU" "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM" )
              "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" ) ) ) ) |}
  in
  let expected =
    Tunac.Values.(
      Union
        (Left
           (Union
              (Left
                 (Union
                    (Right
                       (Pair
                          ( Pair
                              ( String "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
                              , String "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM" )
                          , String "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" ))))))))
  in
  Alcotest.(check @@ result value error)
    "%%getAllowance" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value);

  let unparsed_value =
    {|
      ( Left
        ( Right
          ( Left
             ( Pair "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU" "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" ) ) ) )
    |}
  in
  let expected =
    Tunac.Values.(
      Union
        (Left
           (Union
              (Right
                 (Union
                    (Left
                       (Pair
                          ( String "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
                          , String "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" ))))))))
  in
  Alcotest.(check @@ result value error)
    "%%getBalance" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value);

  let unparsed_value =
    {|
      ( Left
         ( Right
            ( Right
              ( Pair Unit "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK" ) ) ))
    |}
  in
  let expected =
    Tunac.Values.(
      Union
        (Left
           (Union
              (Right
                 (Union
                    (Right
                       (Pair
                          (Unit, String "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK"))))))))
  in
  Alcotest.(check @@ result value error)
    "%%getTotalSupply" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value);

  let unparsed_value =
    {| ( Right ( Pair "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU" ( Pair "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM" 500 ) ) ) |}
  in
  let expected =
    Tunac.Values.(
      Union
        (Right
           (Pair
              ( String "tz1VjdQ5kZpGjk5tH4hADaee9MAd1knsBVSU"
              , Pair
                  ( String "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM"
                  , Int (Z.of_int 500) ) ))))
  in
  Alcotest.(check @@ result value error)
    "%%transfer" (Ok expected)
    (Tunac.Compiler.compile_value unparsed_value)

let () =
  let open Alcotest in
  run "Compile value"
    [ ( "Values"
      , [ test_case "Integers" `Quick integers
        ; test_case "Booleans" `Quick booleans
        ; test_case "Bytes" `Quick bytes_
        ; test_case "Strings" `Quick strings
        ; test_case "Unit" `Quick unit_
        ; test_case "Pairs" `Quick pairs
        ; test_case "Unions" `Quick unions
        ; test_case "Optionals" `Quick optionals
        ; test_case "Lists" `Quick lists
        ; test_case "Maps" `Quick maps
        ] )
    ; ( "Complex values"
      , [ test_case "FA1.2 storage" `Quick fa12_storage
        ; test_case "FA1.2 entrypoints" `Quick fa12_entrypoints
        ] )
    ]
