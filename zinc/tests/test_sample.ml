[@@@warning "-40"]

module Zinc_types = Zinc_types.Raw

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
             ( "None",
               [
                 Core Grab;
                 Plain_old_data (String "Not a contract");
                 Control_flow Failwith;
               ] );
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
  |> Zinc_types.Zinc.to_yojson

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Types.Zinc.of_yojson zinc |> Result.get_ok in
  let run_resutl =
    try eval (initial_state zinc) with Failure x -> Failure x
  in
  Types.Interpreter_output.to_string run_resutl |> print_endline ;
  [%expect
    {|
       interpreting:
       code:  [(Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV));
         (Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
         (Core Grab); (Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   []
       stack: []
       interpreting:
       code:  [(Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
         (Core Grab); (Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   []
       stack: [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       interpreting:
       code:  [(Core (Access 0)); (Domain_specific_operation Contract_opt); (Core Grab);
         (Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: []
       interpreting:
       code:  [(Domain_specific_operation Contract_opt); (Core Grab); (Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       interpreting:
       code:  [(Core Grab); (Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: [(Variant ("None", (Record [||])))]
       interpreting:
       code:  [(Core (Access 0));
         (Adt
            (MatchVariant
               [("Some", [(Core Grab); (Core (Access 0))]);
                 ("None",
                  [(Core Grab); (Plain_old_data (String "Not a contract"));
                    (Control_flow Failwith)])
                 ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Variant ("None", (Record [||])));
         (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: []
       interpreting:
       code:  [(Adt
           (MatchVariant
              [("Some", [(Core Grab); (Core (Access 0))]);
                ("None",
                 [(Core Grab); (Plain_old_data (String "Not a contract"));
                   (Control_flow Failwith)])
                ]));
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Variant ("None", (Record [||])));
         (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: [(Variant ("None", (Record [||])))]
       interpreting:
       code:  [(Core Grab); (Plain_old_data (String "Not a contract"));
         (Control_flow Failwith); (Core EndLet); (Core Grab);
         (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Variant ("None", (Record [||])));
         (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: [(Record [||])]
       interpreting:
       code:  [(Plain_old_data (String "Not a contract")); (Control_flow Failwith);
         (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Record [||]); (Variant ("None", (Record [||])));
         (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: []
       interpreting:
       code:  [(Control_flow Failwith); (Core EndLet); (Core Grab);
         (Plain_old_data (String "my string"));
         (Plain_old_data
            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
         (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
         (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
         (Core Return)]
       env:   [(Record [||]); (Variant ("None", (Record [||])));
         (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
       stack: [(Z (Plain_old_data (String "Not a contract")))]
       (Failure "Not a contract") |}]
