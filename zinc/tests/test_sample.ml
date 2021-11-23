[@@@warning "-40"]

let zinc =
  Zinc_types.Zinc.
    [
      Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
      Core Grab;
      Core (Access 0);
      Domain_specific_operation Contract_opt;
      Core Grab;
      Core (Access 0);
      Adt
        (MatchVariant
           [
             ("Some", [Core Grab; Core (Access 0)]);
             ("None", [Core Grab; Plain_old_data (String "Not a contract"); Control_flow Failwith]);
           ]);
      Core EndLet;
      Core Grab;
      Plain_old_data (String "my string");
      Plain_old_data
        (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
      Core (Access 0);
      Plain_old_data (Mutez (Z.of_int 10));
      Adt (MakeRecord 0);
      Domain_specific_operation MakeTransaction;
      Adt (MakeRecord 3);
      Core Return;
    ]

let%expect_test _ =
  let open Zinc_interpreter in
  let run_resutl =
    try
      interpret_zinc
        {Zinc_types.get_contract_opt = (fun _address -> None)}
        (initial_state zinc)
    with Failure x -> Failure x
  in
  Zinc_types.Interpreter_output.show run_resutl |> print_endline ;
  [%expect
    {|
    interpreting:
    code:  [(Plain_old_data
                             (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"));
                           (Core Grab); (Core (Access 0));
                           (Domain_specific_operation Contract_opt); (Core Grab);
                           (Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   []
    stack:
    []
    interpreting:
    code:  [(Core Grab); (Core (Access 0));
                           (Domain_specific_operation Contract_opt); (Core Grab);
                           (Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   []
    stack:
    [(Zinc_types.Stack_item.Z
        (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
      ]
    interpreting:
    code:  [(Core (Access 0));
                           (Domain_specific_operation Contract_opt); (Core Grab);
                           (Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    []
    interpreting:
    code:  [(Domain_specific_operation Contract_opt); (Core Grab);
                           (Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    [(Zinc_types.Stack_item.Z
        (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
      ]
    interpreting:
    code:  [(Core Grab); (Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    [(Zinc_types.Stack_item.Variant ("None", (Zinc_types.Stack_item.Record [||])
        ))
      ]
    interpreting:
    code:  [(Core (Access 0));
                           (Adt
                              (MatchVariant
                                 [("Some", [(Core Grab); (Core (Access 0))]);
                                   ("None",
                                    [(Core Grab);
                                      (Plain_old_data (String "Not a contract"));
                                      (Control_flow Failwith)])
                                   ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Variant (
                                                                        "None",
                                                                        (Zinc_types.Stack_item.Record
                                                                        [||])));
                                                                        (Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    []
    interpreting:
    code:  [(Adt
                             (MatchVariant
                                [("Some", [(Core Grab); (Core (Access 0))]);
                                  ("None",
                                   [(Core Grab);
                                     (Plain_old_data (String "Not a contract"));
                                     (Control_flow Failwith)])
                                  ]));
                           (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Variant (
                                                                        "None",
                                                                        (Zinc_types.Stack_item.Record
                                                                        [||])));
                                                                        (Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    [(Zinc_types.Stack_item.Variant ("None", (Zinc_types.Stack_item.Record [||])
        ))
      ]
    interpreting:
    code:  [(Core Grab);
                           (Plain_old_data (String "Not a contract"));
                           (Control_flow Failwith); (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Variant (
                                                                        "None",
                                                                        (Zinc_types.Stack_item.Record
                                                                        [||])));
                                                                        (Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    [(Zinc_types.Stack_item.Record [||])]
    interpreting:
    code:  [(Plain_old_data (String "Not a contract"));
                           (Control_flow Failwith); (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Record
                                                                        [||]);
                                                                        (Zinc_types.Env_item.Variant (
                                                                        "None",
                                                                        (Zinc_types.Stack_item.Record
                                                                        [||])));
                                                                        (Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    []
    interpreting:
    code:  [(Control_flow Failwith); (Core EndLet); (Core Grab);
                           (Plain_old_data (String "my string"));
                           (Plain_old_data
                              (Key
                                 "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"));
                           (Core (Access 0)); (Plain_old_data (Mutez 10));
                           (Adt (MakeRecord 0));
                           (Domain_specific_operation MakeTransaction);
                           (Adt (MakeRecord 3)); (Core Return)]
    env:   [(Zinc_types.Env_item.Record
                                                                        [||]);
                                                                        (Zinc_types.Env_item.Variant (
                                                                        "None",
                                                                        (Zinc_types.Stack_item.Record
                                                                        [||])));
                                                                        (Zinc_types.Env_item.Z
                                                                        (Plain_old_data
                                                                        (Address
                                                                        "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))
                                                                        ]
    stack:
    [(Zinc_types.Stack_item.Z (Plain_old_data (String "Not a contract")))]
    (Zinc_types.Interpreter_output.Failure "Not a contract") |}]
