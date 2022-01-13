[@@@warning "-40"]

open Zinc_interpreter.Dummy
module Zinc_types = Ir.Zt

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
  |> Zinc_types.Zinc.to_yojson

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fun.id

  let key_hash s = s ^ "hash"
end

let%test _ =
  let open Zinc_interpreter.Dummy in
  let code = Ir.Num Z.one in
  let written = Bin_prot.Utils.bin_dump Ir.bin_writer_t code in
  let[@warning "-8"] (Ir.Num x) = Ir.bin_read_t ~pos_ref:(ref 0) written in
  Z.equal x Z.one

let%test _ =
  let open Zinc_interpreter.Dummy in
  let code = Ir.List [Ir.String "test"; Ir.Num Z.one] in
  let code2 = Ir.String "test" in
  not (Int.equal (Ir.bin_size_t code) (Ir.bin_size_t code2))

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Zinc_types.Zinc.of_yojson zinc |> Result.get_ok |> Ir.of_typed in
  let (env, stack) = eval' (module Executor) ~debug:false (zinc, [], []) in
  let str =
    Format.asprintf
      "Env: %s\nStack:%s\n"
      ([%derive.show: Ir.t list] env)
      ([%derive.show: Ir.t list] stack)
  in
  print_endline str ;
  [%expect
    {|
    Env: [(Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]);
      Variant {tag = 0;
        value = (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null])};
      (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)]
    Stack:[(Record
        [|(Transaction (10, ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
          (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav);
          (String "my string")|])
      ] |}]

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Zinc_types.Zinc.of_yojson zinc |> Result.get_ok in
  let run_resutl = eval (module Executor) (initial_state zinc) in
  Zinc_types.Interpreter_output.to_string run_resutl |> print_endline ;
  [%expect
    {|
             (Success (
                [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
                  (Variant (0,
                     (NonliteralValue
                        (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
                     ));
                  (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))],
                [(Record
                    [|(NonliteralValue
                         (Chain_operation
                            (Transaction (10,
                               ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))));
                      (Z
                         (Plain_old_data
                            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
                      (Z (Plain_old_data (String "my string")))|])
                  ]
                )) |}]
