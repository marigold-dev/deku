module Contract_address = Deku_ledger.Contract_address
module Address = Deku_ledger.Address
module Ledger = Deku_ledger.Ledger
open Deku_stdlib

type t = {
  sender : Address.t;
  source : Address.t;
  mutable ledger : Ledger.t;
  mutable state : State.t;
  mutable ticket_table : Ticket_table.t;
}

let make ~state ~ledger ~sender ~source =
  { sender; source; ledger; state; ticket_table = Ticket_table.init [] }

let rec execute t ~operation_hash ~tickets ~operation =
  let _ = t in
  let open Effect.Deep in
  let open Effects in
  let call_indirect = ref (fun _ _ -> assert false) in
  let call_indirect_unit = ref (fun _ _ -> assert false) in
  let pop = ref (fun _ -> assert false) in
  let push = ref (fun _ -> assert false) in
  let constants = ref [||] in
  let%ok self =
    try
      match operation with
      | Operation.Call { address; _ } ->
          let res =
            Deku_ledger.Address.to_contract_address address
            |> Option.map fst |> Option.get
          in
          Ok (ref (fun () -> res))
      | _ -> Ok (ref (fun () -> failwith "lifecycle error"))
    with Invalid_argument s -> Error s
  in
  let%ok state, ops =
    try
      Effect.Deep.match_with
        (Flow.apply operation_hash t.state t.ticket_table tickets)
        operation
        Effect.
          {
            retc = (fun x -> Ok x);
            exnc =
              (function
              | Flow.Runtime l -> Error (Flow.show_error l)
              | Imports.Type_error -> Error "type_error"
              | Ticket_table.Table error ->
                  Error (Ticket_table.show_error error)
              | Wasm.Eval.Crash (source_region, message)
              | Wasm.Eval.Exhaustion (source_region, message)
              | Wasm.Eval.Link (source_region, message)
              | Wasm.Eval.Trap (source_region, message) ->
                  Error
                    (Format.sprintf "%s: %s"
                       (Wasm.Source.string_of_region source_region)
                       message)
              | e -> raise e);
            effc =
              (fun (type a) (eff : a t) ->
                match eff with
                | Effects.Deposit_tickets (addr, tickets) ->
                    let ledger =
                      Ledger.with_ticket_table t.ledger
                        (fun ~get_table ~set_table ->
                          let table =
                            List.fold_left
                              (fun acc (ticket_id, amount) ->
                                Deku_ledger.Ticket_table.deposit acc
                                  ~destination:addr ~ticket_id
                                  ~amount:(Deku_concepts.Amount.of_n amount))
                              (get_table ()) tickets
                          in
                          set_table table)
                    in
                    t.ledger <- ledger;
                    Some (fun (k : (a, _) continuation) -> continue k ())
                | Take_tickets addr ->
                    let ledger, tickets =
                      Ledger.with_ticket_table t.ledger
                        (fun ~get_table ~set_table ->
                          let tickets, table =
                            Deku_ledger.Ticket_table.take_all_tickets
                              (get_table ()) ~sender:addr
                          in
                          ( set_table table,
                            Seq.fold_left
                              (fun acc (id, amount) ->
                                (id, Deku_concepts.Amount.to_n amount) :: acc)
                              [] tickets ))
                    in
                    t.ledger <- ledger;
                    Some (fun (k : (a, _) continuation) -> continue k tickets)
                | Self_addr ->
                    Some
                      (fun (k : (a, _) continuation) -> continue k (!self ()))
                | Sender_address ->
                    Some (fun (k : (a, _) continuation) -> continue k t.sender)
                | Source_address ->
                    Some (fun (k : (a, _) continuation) -> continue k t.source)
                | Push_to_contract_stack x ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        continue k (!push x : unit))
                | Pop ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        continue k (!pop () : int64))
                | Indirect_call ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        continue k !call_indirect)
                | Indirect_call_unit ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        continue k !call_indirect_unit)
                | Get_constant const ->
                    let res : Value.t =
                      Array.get !constants (Int64.to_int const) |> snd
                    in
                    Some (fun (k : (a, _) continuation) -> continue k res)
                | Read_ticket handle ->
                    let ticket_id, amount, handle =
                      Ticket_table.read_ticket t.ticket_table handle
                    in
                    Some (fun k -> continue k (ticket_id, amount, handle))
                | Mint_ticket (ticket_id, amount) ->
                    let handle =
                      Ticket_table.mint_ticket t.ticket_table ticket_id amount
                    in
                    Some (fun k -> continue k handle)
                | Join_ticket (handle1, handle2) ->
                    let handle : int option =
                      Ticket_table.join_tickets t.ticket_table (handle1, handle2)
                    in
                    Some (fun k -> continue k handle)
                | Split_ticket (handle, amounts) ->
                    let handle =
                      Ticket_table.split_ticket t.ticket_table handle amounts
                    in
                    Some (fun k -> continue k handle)
                | Set_constants constants' ->
                    constants := constants';
                    Some (fun k -> continue k ())
                | Set_instance instance ->
                    (call_indirect :=
                       fun idx arg ->
                         let func =
                           Externs.(extract_func instance Externs.call_callback)
                           |> Result.get_ok
                         in
                         match
                           Wasm.Eval.invoke func
                             (arg @ [ Wasm.Values.(Num (I32 idx)) ])
                         with
                         | Wasm.Values.[ Num (I64 x) ] -> x
                         | _ -> failwith "bad indirect call in contract");
                    (call_indirect_unit :=
                       fun idx arg ->
                         let func =
                           Externs.(
                             extract_func instance Externs.call_callback_unit)
                           |> Result.get_ok
                         in
                         let _ =
                           Wasm.Eval.invoke func
                             (arg @ [ Wasm.Values.(Num (I32 idx)) ])
                         in
                         ());
                    (push :=
                       fun arg ->
                         let func =
                           Externs.(extract_func instance Externs.push)
                           |> Result.get_ok
                         in
                         ignore @@ Wasm.Eval.invoke func [ arg ]);
                    (pop :=
                       fun _ ->
                         let func =
                           Externs.(extract_func instance Externs.pop)
                           |> Result.get_ok
                         in
                         match Wasm.Eval.invoke func [] with
                         | Wasm.Values.[ Num (I64 x) ] -> x
                         | _ -> failwith "bad indirect call in contract");
                    Some (fun (k : (a, _) continuation) -> continue k ())
                | _ -> None);
          }
    with
    | Failure s -> Error ("unexpected failure. Failure " ^ s)
    | Wasm.Eval.Crash (_, s) ->
        Error ("Crash: Invalid contract supplied to the vm: " ^ s)
    | Wasm.Eval.Exhaustion (_, s) ->
        Error ("Exhaustion: Invalid contract supplied to the vm: " ^ s)
    | Wasm.Eval.Link (_, s) ->
        Error ("Link: Invalid contract supplied to the vm: " ^ s)
    | Wasm.Eval.Trap (_, s) ->
        Error ("Trap: Invalid contract supplied to the vm: " ^ s)
    | Ticket_table.Table err ->
        Error
          (Format.sprintf "Ticket table Error: %s"
             (Ticket_table.show_error err))
    | Invalid_argument s -> Error ("unexpected failure. Argument " ^ s)
  in
  t.state <- state;
  let%ok new_state =
    try
      let res =
        List.fold_left
          (fun acc (op, tickets) ->
            let open Deku_ledger in
            let open Value in
            match op with
            | Pair (String addr, param) -> (
                match Address.of_b58 addr with
                | Some x when Option.is_none @@ Address.to_contract_address x ->
                    let ledger =
                      Ledger.with_ticket_table acc.ledger
                        (fun ~get_table ~set_table ->
                          let ledger =
                            List.fold_left
                              (fun acc (ticket_id, amount) ->
                                Ticket_table.deposit acc ~destination:x
                                  ~ticket_id
                                  ~amount:(Deku_concepts.Amount.of_n amount))
                              (get_table ()) tickets
                          in
                          let ledger = set_table ledger in
                          ledger)
                    in
                    acc.ledger <- ledger;
                    acc
                | Some x -> (
                    let result =
                      execute
                        {
                          acc with
                          sender = Address.of_contract_address (!self (), None);
                        }
                        ~operation_hash ~tickets
                        ~operation:
                          (Operation.Call { address = x; argument = param })
                    in
                    match result with Error x -> failwith x | Ok x -> x)
                | None -> failwith "bad address for transaction")
            | _ -> failwith "bad additional operation")
          t ops
      in
      Ok res
    with Failure s -> Error s
  in
  Ok new_state

let finalize = function Ok t -> Ok (t.state, t.ledger) | Error _ as x -> x
