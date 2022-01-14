[@@@warning "-40"]

open Zinc_utils
module Zinc_interpreter = Zinc_interpreter.Dummy
open Zinc_interpreter.Types
open Zinc_interpreter
open Zinc

let zinc =
  let open Z in
  [
    Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
    Core Grab;
    Core (Access 0);
    Domain_specific_operation Contract_opt;
    Core Grab;
    Core (Access 0);
    Adt
      (MatchVariant
         [|
           [Core Grab; Core (Access 0)];
           [
             Core Grab;
             Plain_old_data (String "Not a contract");
             Control_flow Failwith;
           ];
         |]);
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
  |> Zinc.to_yojson

module Executor : Executor = struct
  let get_contract_opt a = Some Contract.{address = a; entrypoint = None}

  let chain_id = "chain id goes here"

  let key_hash s = s ^ "hash"
end

let%expect_test _ =
  let open Interpreter in
  let zinc = Types.Zinc.of_yojson zinc |> Result.get_ok in
  let run_result = eval (module Executor) (initial_state zinc) in
  Types.Interpreter_output.to_string run_result |> print_endline ;
  [%expect
    {|
    interpreting:
    code:  [(Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"));
      (Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
      (Core Grab); (Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   []
    stack: []
    interpreting:
    code:  [(Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
      (Core Grab); (Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   []
    stack: [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    interpreting:
    code:  [(Core (Access 0)); (Domain_specific_operation Contract_opt); (Core Grab);
      (Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: []
    interpreting:
    code:  [(Domain_specific_operation Contract_opt); (Core Grab); (Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    interpreting:
    code:  [(Core Grab); (Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ))
      ]
    interpreting:
    code:  [(Core (Access 0));
      (Adt
         (MatchVariant
            [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
              [(Core Grab); (Plain_old_data (String "Not a contract"));
                (Control_flow Failwith); (Core EndLet)]
              |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: []
    interpreting:
    code:  [(Adt
        (MatchVariant
           [|[(Core Grab); (Core (Access 0)); (Core EndLet)];
             [(Core Grab); (Plain_old_data (String "Not a contract"));
               (Control_flow Failwith); (Core EndLet)]
             |]));
      (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ))
      ]
    interpreting:
    code:  [(Core Grab); (Core (Access 0)); (Core EndLet); (Core EndLet); (Core Grab);
      (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }))
      ]
    interpreting:
    code:  [(Core (Access 0)); (Core EndLet); (Core EndLet); (Core Grab);
      (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Variant (0,
         (NonliteralValue
            (Contract
               { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                 entrypoint = None }))
         ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: []
    interpreting:
    code:  [(Core EndLet); (Core EndLet); (Core Grab);
      (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Variant (0,
         (NonliteralValue
            (Contract
               { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                 entrypoint = None }))
         ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }))
      ]
    interpreting:
    code:  [(Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Variant (0,
        (NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }))
        ));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }))
      ]
    interpreting:
    code:  [(Core Grab); (Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }))
      ]
    interpreting:
    code:  [(Plain_old_data (String "my string"));
      (Plain_old_data
         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: []
    interpreting:
    code:  [(Plain_old_data (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
      (Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Core (Access 0)); (Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Z
        (Plain_old_data
           (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
      (Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Plain_old_data (Mutez 0)); (Adt (MakeRecord 0));
      (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
      (Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Adt (MakeRecord 0)); (Domain_specific_operation MakeTransaction);
      (Adt (MakeRecord 3)); (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Z (Plain_old_data (Mutez 0)));
      (NonliteralValue
         (Contract
            { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
              }));
      (Z
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
      (Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
      (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Record [||]); (Z (Plain_old_data (Mutez 0)));
      (NonliteralValue
         (Contract
            { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
              }));
      (Z
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
      (Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Adt (MakeRecord 3)); (Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(NonliteralValue
        (Chain_operation
           (Transaction ((Record [||]),
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }
              ))));
      (Z
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
      (Z (Plain_old_data (String "my string")))]
    interpreting:
    code:  [(Core Return)]
    env:   [(NonliteralValue
        (Contract
           { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"; entrypoint = None
             }));
      (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))]
    stack: [(Record
        [|(NonliteralValue
             (Chain_operation
                (Transaction ((Record [||]),
                   { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                     entrypoint = None }
                   ))));
          (Z
             (Plain_old_data
                (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
          (Z (Plain_old_data (String "my string")))|])
      ]
    (Success (
       [(NonliteralValue
           (Contract
              { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                entrypoint = None }));
         (Z (Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV")))],
       [(Record
           [|(NonliteralValue
                (Chain_operation
                   (Transaction ((Record [||]),
                      { address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
                        entrypoint = None }
                      ))));
             (Z
                (Plain_old_data
                   (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
             (Z (Plain_old_data (String "my string")))|])
         ]
       )) |}]
