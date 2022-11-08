let value' =
  Alcotest.testable Ocaml_wasm_vm.Value.pp (fun a b ->
      Ocaml_wasm_vm.Value.compare a b = 0)

let initial_storage =
  let json =
    Data_encoding.Json.from_string
      {|
      [
        "Pair",
        [
            [
                "Pair",
                [
                    [
                        "List",
                        [
                            [
                                "Union",
                                [
                                    "Left",
                                    [
                                        "Union",
                                        [
                                            "Right",
                                            [
                                                "Unit"
                                            ]
                                        ]
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Left",
                                    [
                                        "Union",
                                        [
                                            "Left",
                                            [
                                                "Unit"
                                            ]
                                        ]
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ],
                            [
                                "Union",
                                [
                                    "Right",
                                    [
                                        "Unit"
                                    ]
                                ]
                            ]
                        ]
                    ],
                    [
                        "Union",
                        [
                            "Left",
                            [
                                "String",
                                "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
                            ]
                        ]
                    ]
                ]
            ],
            [
                "Pair",
                [
                    [
                        "String",
                        "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
                    ],
                    [
                        "String",
                        "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
                    ]
                ]
            ]
        ]
    ]
  |}
  in
  json |> Result.get_ok
  |> Data_encoding.Json.destruct Ocaml_wasm_vm.Value.encoding

let originate =
  let open Ocaml_wasm_vm in
  let open Value in
  let module_ =
    "0061736d0100000001c3808080000d60017e017e60017e0060017e017f60027e7e017e60017f017e6000017e60027e7f0060037e7e7e017e60027e7f017e60027f7e017e60037e7e7e0060017f00600000028a888080005103656e760769735f6c656674000203656e76086475705f686f7374000103656e760470616972000303656e7606706169725f6e000403656e7606756e70616972000103656e76057a5f616464000303656e76057a5f737562000303656e76057a5f6d756c000303656e76036e6567000003656e76036c736c000303656e7606636f6e636174000303656e76036c7372000303656e7607636f6d70617265000303656e7603636172000003656e7603636472000003656e7604736f6d65000003656e76036e696c000503656e760474727565000503656e7608756e706169725f6e000603656e760566616c7365000503656e76046e6f6e65000503656e7604756e6974000503656e76047a65726f000503656e7609656d7074795f6d6170000503656e7609656d7074795f736574000503656e760d656d7074795f6269675f6d6170000503656e760673656e646572000503656e7606736f75726365000503656e76076d61705f676574000303656e76036d656d000303656e7606757064617465000703656e760469746572000603656e76036d6170000803656e760769665f6c656674000203656e760769665f6e6f6e65000203656e760769665f636f6e73000203656e760569736e6174000003656e76036e6f74000003656e76026f72000303656e7603616e64000303656e7603786f72000303656e760a64657265665f626f6f6c000203656e76036e6571000003656e76086661696c77697468000103656e76056765745f6e000903656e760465786563000303656e76056170706c79000303656e7605636f6e7374000403656e7603616273000003656e76026571000003656e76026774000003656e76026c74000003656e7607636c6f73757265000403656e76046c656674000003656e76057269676874000003656e7604636f6e73000303656e760f7472616e736665725f746f6b656e73000703656e760761646472657373000003656e7608636f6e7472616374000003656e760473656c66000503656e760c73656c665f61646472657373000503656e760e6765745f616e645f757064617465000a03656e760b726561645f7469636b6574000103656e76067469636b6574000303656e760c6a6f696e5f7469636b657473000003656e760c73706c69745f7469636b6574000303656e7606616d6f756e74000503656e760762616c616e6365000503656e760465646976000303656e76026765000003656e76026c65000003656e760473697a65000003656e7603696e74000003656e7610696d706c696369745f6163636f756e74000003656e7607626c616b653262000003656e76047061636b000003656e7606756e7061636b000003656e76066b656363616b000003656e7606736861323536000003656e760473686133000003656e76067368613531320000038e808080000d08060b0b0b0c0b0b05010b00000485808080000170010101058380808000010004069980808000047f0041000b7f0141a01f0b7f0141e8070b7f00418080020b07c580808000060470757368005a03706f700059046d61696e005d08636c6f737572657301000d63616c6c5f63616c6c6261636b00511263616c6c5f63616c6c6261636b5f756e69740052098780808000010041000b015c0acf888080000d898080800000200020011100000b898080800000200020011101000bc48080800001037f4100210123012102230220006b22032402034041082303200320016a6a6c4108200220016a6c290300370300200141016a22012000470d000b200220006a24010bc48080800001037f230120006b22022401230221034100210103404108200220016a6c23034108200320016a6c6a290300370300200141016a22012000470d000b200320006a24020b8f80808000004108230120006a6c29030010010b948080800001027e10592100105921012000105a2001105a0bcb8080800002037f017e230120006a210323012201220241086c29030021040340410820016c200241016a220241086c290300370300200141016a210120012003490d000b410820036c20043703000bc28080800002027f017e4108230120006a22016c29030021030340410820016c210220024108200141016b22016c29030037030023012001490d000b410820016c20033703000b958080800001017f4108230122006c290300200041016a24010b978080800001017f4108230141016b22016c2000370300200124010b898080800000230120006a24010bbc8180800001017e2000105a1059100410594103101241031058105910044103105810591059100c105a1059102a105a1059102904401056410210584102105b051059102104401056410210584102105b1059102104404101105b4106102f105a1059102b00054101105b4107102f105a1059102b000b054101105b10561059100e105a1059100d105a105610591059100c105a10591031105a1059102904401015105a10591036105a051015105a10591035105a0b10591035105a0b0b10590bd28380800001017e2000105a105910041059102104404102105b4100102f105a1059102b00054101102f105a41021055105910041059100e105a410410551059100d105a1059100e105a105910210440054101105b4102102f105a1059102b000b41031055105610591059100c105a1059102a105a1059102904404105105b4103102f105a1059102b00054104102f105a4104105510591059100c105a10591032105a1059102904404105105b4105102f105a1059102b00051010105a410510551059100d105a1059100d105a105910591002105a1016105a41001034105a41071055410710554107105541031003105a10591059102e105a4105105841061058410710584103105b105910591002105a105910591002105a10591035105a03401059102104401059100410591004410210581059100410591023044041041055105910591002105a41031055105610591059102d105a410210581056105910591037105a1056105910591002105a4108102f105a41031058105910591005105a41021058105910591002105a105910591002105a10591035105a051056410210584102105b10591036105a0b105910000d010b0b105910591002105a105910591002105a0b0b0b1010105a105910591002105a10590b"
  in
  let json =
    Data_encoding.Json.from_string
      {|{ 
        "%join": [ "Left" ], 
        "%play": [ "Right" ] 
        } 
      |}
  in
  let json = json |> Result.get_ok in
  let entrypoints = Data_encoding.Json.destruct Entrypoints.encoding json in
  let constants =
    [|
      (0, String "Join is not yet implemented");
      (1, String "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb");
      (2, String "Game is ended");
      (3, String "Not your turn");
      (4, Int (Z.of_int 8));
      (5, String "Wrong cellId");
      (6, String "Already set: circle");
      (7, String "Already set: cross");
      (8, Int (Z.of_int 1));
    |]
  in
  Operation.Originate { initial_storage; module_; entrypoints; constants }

let invoke =
  let open Ocaml_wasm_vm in
  let open Deku_ledger in
  let res =
    Operation.Call
      {
        address =
          Address.of_contract_address
            ( Contract_address.of_user_operation_hash
                (Deku_crypto.BLAKE2b.hash "tutturu"),
              None );
        argument = Union (Right (Int (Z.of_int 0)));
      }
  in
  res

let new_address () =
  let open Deku_crypto in
  let open Deku_ledger in
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  let key = Key.of_secret secret in
  let key_hash = Key_hash.of_key key in
  Address.of_key_hash key_hash

let () =
  let open Alcotest in
  run "Wasm-vm" ~and_exit:false ~verbose:true
    [
      ( "Basic Vm tests",
        [
          test_case "Originate" `Quick (fun () ->
              let open Ocaml_wasm_vm in
              let addr = new_address () in
              let x =
                Env.execute
                  ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                  ~tickets:[] ~operation:originate
                  Env.
                    {
                      source = addr;
                      sender = addr;
                      ledger = Deku_ledger.Ledger.initial;
                      state = State.empty;
                      ticket_table = Ticket_table.init [];
                    }
              in
              print_endline "origination...";
              let state = Result.get_ok x in
              print_endline "origination success";
              let (State_entry.Entry { storage; _ }) =
                State.fetch_contract state.state
                  Deku_ledger.(
                    Contract_address.of_user_operation_hash
                      (Deku_crypto.BLAKE2b.hash "tutturu"))
              in
              Alcotest.(check value')
                "storage should be equal" initial_storage storage;

              (********)
              let x =
                Env.execute
                  ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                  ~tickets:[] ~operation:invoke
                  Env.
                    {
                      source = addr;
                      sender = addr;
                      ledger = Deku_ledger.Ledger.initial;
                      state = state.state;
                      ticket_table = Ticket_table.init [];
                    }
              in
              let state = Result.get_ok x in
              print_endline "invokation is success";
              let (State_entry.Entry { storage; _ }) =
                State.fetch_contract state.state
                  Deku_ledger.(
                    Contract_address.of_user_operation_hash
                      (Deku_crypto.BLAKE2b.hash "tutturu"))
              in
              Alcotest.(check (neg value'))
                "storage should have change" initial_storage storage);
        ] );
    ]

(*
           test_case "Originate/Invoke increment" `Quick (fun () ->
               let open Alcotest in
               let open Ocaml_wasm_vm in
               let addr = new_address () in
               let x =
                 Env.execute
                   ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                   ~tickets:[]
                   Env.
                     {
                       source = addr;
                       sender = addr;
                       ledger = Deku_ledger.Ledger.initial;
                       state = State.empty;
                       ticket_table = Ticket_table.init [];
                     }
                   ~operation:originate
               in
               let state = Result.get_ok x in
               let (State_entry.Entry { storage; _ }) =
                 State.fetch_contract state.state
                   Deku_ledger.(
                     Contract_address.of_user_operation_hash
                       (Deku_crypto.BLAKE2b.hash "tutturu"))
               in
               (check bool) "Invoke" true (storage = Int Z.zero);
               let x =
                 Env.execute
                   ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                   ~tickets:[]
                   Env.
                     {
                       source = addr;
                       sender = addr;
                       ledger = Deku_ledger.Ledger.initial;
                       state = state.state;
                       ticket_table = Ticket_table.init [];
                     }
                   ~operation:invoke
               in
               let state = Result.get_ok x in
               let (State_entry.Entry { storage; _ }) =
                 State.fetch_contract state.state
                   Deku_ledger.(
                     Contract_address.of_user_operation_hash
                       (Deku_crypto.BLAKE2b.hash "tutturu"))
               in
               (check bool) "example" true (storage = Int (Z.of_int 5));
               ());
               ] );
               ]
*)
