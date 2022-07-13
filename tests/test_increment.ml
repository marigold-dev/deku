open Core_deku
open Crypto

let test () =
  let state = Core_deku.State.empty in

  let source =
    Key_hash.of_string "tz1U4btBuSjXzcMrXc7mxRyvuGz1gXZ37NUE" |> Option.get
  in

  let operation = User_operation.make ~source (User_operation.Increment 3) in

  let operation_hash = BLAKE2B.hash "hello" in

  let state', _receipt =
    State.apply_user_operation state operation_hash operation in

  let counter = State.get_counter state' in

  assert (counter = 3)
