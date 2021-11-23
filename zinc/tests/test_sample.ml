[@@@warning "-40"]

let zinc =
  Zinc_types.Zinc.
    [
      Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
      Grab;
      Access 0;
      Contract_opt;
      Grab;
      Access 0;
      MatchVariant
        [
          ("Some", [Grab; Access 0]);
          ("None", [Grab; String "Not a contract"; Failwith]);
        ];
      EndLet;
      Grab;
      String "my string";
      Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
      Access 0;
      Mutez (Z.of_int 10);
      MakeRecord 0;
      MakeTransaction;
      MakeRecord 3;
      Return;
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
    code:  [(Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"); Grab;
                           (Access 0); Contract_opt; Grab; (Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   []
    stack: []
    interpreting:
    code:  [Grab; (Access 0); Contract_opt; Grab; (Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   []
    stack: [(Zinc_types.Zinc.Stack_item.Z
                                                        (Address
                                                           "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                                      ]
    interpreting:
    code:  [(Access 0); Contract_opt; Grab; (Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Z
                                              (Address
                                                 "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: []
    interpreting:
    code:  [Contract_opt; Grab; (Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Z
                                              (Address
                                                 "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: [(Zinc_types.Zinc.Stack_item.Z
                                                         (Address
                                                            "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                                       ]
    interpreting:
    code:  [Grab; (Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Z
                                              (Address
                                                 "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: [(Zinc_types.Zinc.Stack_item.Variant (
                                                         (Label "None"),
                                                         (Zinc_types.Zinc.Stack_item.Record
                                                            )
                                                         ))
                                                       ]
    interpreting:
    code:  [(Access 0);
                           (MatchVariant
                              [((Label "Some"), [Grab; (Access 0)]);
                                ((Label "None"),
                                 [Grab; (String "Not a contract"); Failwith])
                                ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Variant (
                                              (Label "None"),
                                              (Zinc_types.Zinc.Stack_item.Record )));
                                            (Zinc_types.Zinc.Env_item.Z
                                               (Address
                                                  "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: []
    interpreting:
    code:  [(MatchVariant
                             [((Label "Some"), [Grab; (Access 0)]);
                               ((Label "None"),
                                [Grab; (String "Not a contract"); Failwith])
                               ]);
                           EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Variant (
                                              (Label "None"),
                                              (Zinc_types.Zinc.Stack_item.Record )));
                                            (Zinc_types.Zinc.Env_item.Z
                                               (Address
                                                  "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: [(Zinc_types.Zinc.Stack_item.Variant (
                                                         (Label "None"),
                                                         (Zinc_types.Zinc.Stack_item.Record
                                                            )
                                                         ))
                                                       ]
    interpreting:
    code:  [Grab; (String "Not a contract"); Failwith; EndLet;
                           Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Variant (
                                              (Label "None"),
                                              (Zinc_types.Zinc.Stack_item.Record )));
                                            (Zinc_types.Zinc.Env_item.Z
                                               (Address
                                                  "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: [(Zinc_types.Zinc.Stack_item.Record
                                                         )
                                                       ]
    interpreting:
    code:  [(String "Not a contract"); Failwith; EndLet; Grab;
                           (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Record );
                                            (Zinc_types.Zinc.Env_item.Variant (
                                               (Label "None"),
                                               (Zinc_types.Zinc.Stack_item.Record )));
                                            (Zinc_types.Zinc.Env_item.Z
                                               (Address
                                                  "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: []
    interpreting:
    code:  [Failwith; EndLet; Grab; (String "my string");
                           (Key
                              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
                           (Access 0); (Mutez 10); (MakeRecord []);
                           MakeTransaction;
                           (MakeRecord [(Label "0"); (Label "1"); (Label "2")]);
                           Return]
    env:   [(Zinc_types.Zinc.Env_item.Record );
                                            (Zinc_types.Zinc.Env_item.Variant (
                                               (Label "None"),
                                               (Zinc_types.Zinc.Stack_item.Record )));
                                            (Zinc_types.Zinc.Env_item.Z
                                               (Address
                                                  "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"))
                                            ]
    stack: [(Zinc_types.Zinc.Stack_item.Z
                                                         (String "Not a contract"))
                                                       ]
    (Zinc_types.Interpreter_output.Failure "Not a contract") |}]
