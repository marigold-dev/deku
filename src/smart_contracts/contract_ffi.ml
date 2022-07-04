open Helpers
open Conversions
open Core

let get_or_raise t =
  Result.map_error
    ~f:(fun e ->
      Format.asprintf "%a" Ticket_transition_table.Errors.pp
        (e :> Ticket_transition_table.Errors.t))
    t
  |> Result.ok_or_failwith

include State

module Addressing = struct
  let sender t = t#sender

  let self t = t#self

  let source t = t#source

  let get_contract_opt t address =
    if Address.is_implicit address then
      Some address
    else
      t#get_contract_opt address |> Option.map ~f:(Fn.const address)
end

module Table_ops = struct
  let read_ticket t ticket_handle =
    let sender = t#self in
    let table = t#table in
    let ticket, amount, handle =
      Ticket_transition_table.read_ticket ~sender ~ticket_handle table
      |> get_or_raise in
    (ticket, amount, handle)

  let own_ticket t ticket_handle =
    let table = t#table in
    let ticket_handle =
      Ticket_transition_table.own table t#self ticket_handle |> get_or_raise
    in
    ticket_handle

  let split_ticket t (handle, amount1, amount2) =
    let sender = t#self in
    let table = t#table in
    let handle1, handle2 =
      Ticket_transition_table.split_ticket ~ticket_handle:handle ~sender
        ~amounts:(amount1, amount2) table
      |> get_or_raise in

    (handle1, handle2)

  let join_tickets t handles =
    let sender = t#self in
    let table = t#table in
    let handle =
      Ticket_transition_table.join_tickets ~sender ~handles table
      |> get_or_raise in
    handle

  let mint_ticket t (value, amount) =
    let sender = t#self in
    let table = t#table in
    Ticket_transition_table.mint_ticket table ~sender ~amount value
end

module Operations = struct
  let transaction t (param, (ticket_handle, amount, handle_addr), address) =
    let sender = t#self in
    let table = t#table in
    let ticket, amount2, _handle =
      Ticket_transition_table.read_ticket ~sender ~ticket_handle table
      |> get_or_raise in
    if not (Amount.equal amount amount2) then failwith "execution error";
    let operation =
      if Address.is_implicit address then
        Operation.Transfer { ticket; amount; destination = address }
      else
        Operation.Invoke
          {
            param;
            tickets = [((ticket, amount), (ticket_handle, handle_addr))];
            destination = address;
          } in
    t#add_operation operation
end

class addressable ~self ~sender ~source ~get_contract_opt =
  object
    inherit State.addressing

    method self = self

    method sender = sender

    method source = source

    method get_contract_opt address = get_contract_opt address
  end

let make_state ~get_contract_opt ~source ~sender ~mapping ~self
    ~contract_owned_tickets ~provided_tickets =
  let ops = ref [] in

  let push operation =
    let idx, operations =
      match !ops with
      | [] -> (1, [(1, operation)])
      | x :: xs ->
        let num, _ = x in
        let new_idx = num + 1 in
        let new_ops = (num + 1, operation) :: x :: xs in
        (new_idx, new_ops) in
    ops := operations;
    idx in
  let to_be_replaced = ref None in
  ( object
      inherit State.full_state

      inherit addressable ~self ~source ~sender ~get_contract_opt

      val table =
        let table, to_replace_tickets =
          Ticket_transition_table.init ~mapping ~self
            ~tickets:contract_owned_tickets ~temporary_tickets:provided_tickets
        in
        to_be_replaced := to_replace_tickets;
        table

      method table = table

      method finalize received =
        let finalized = Ticket_transition_table.finalize table in
        let final = finalized |> Seq.map (fun (_, d, v) -> (d, v)) in

        let saved_tickets =
          finalized
          |> Stdlib.List.of_seq
          |> List.map ~f:(fun (key, t, a) -> ((t, a), key)) in
        let deduped = Set.stable_dedup_list (module Int) received in
        let%ok ops =
          if List.equal Int.equal received deduped then
            let operations = !ops in
            let ops =
              List.fold_left
                ~f:(fun acc x ->
                  let elem = List.Assoc.find_exn ~equal:Int.equal operations x in
                  elem :: acc)
                ~init:[] received in
            Result.return ops
          else
            Result.fail `Execution_error in

        Ok (saved_tickets, final, ops)

      method add_operation operation = push operation
    end,
    !to_be_replaced )
