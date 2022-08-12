open Deku_concepts
open Deku_stdlib
open Contracts

(*TODO: should it be defined in Amount module ?*)
let amount_of_int_or_zero int =
  int |> Z.of_int |> N.of_z
  |> Stdlib.Option.map Amount.of_n
  |> Option.value ~default:Amount.zero

let rec apply ~sender ~operation_hash ~level ~nonce ~apply_operation_content
    ledger contract_storage operation =
  let _ = level in
  let _ = nonce in
  match operation with
  | Contract_operation.Contract_origination { payload; tickets } ->
      (* @TODO: deduct gas from account and check *)
      let balance = Core.Int.max_value |> amount_of_int_or_zero in
      (* TODO: those constants should not be defined here *)
      let origination_cost = 250 |> amount_of_int_or_zero in
      let%assert () =
        Amount.
          ( `Origination_error ("Not enought funds", sender),
            let comparison_result = compare balance origination_cost in
            comparison_result >= 0 )
      in
      let initial_gas =
        Amount.(balance - origination_cost)
        |> Stdlib.Option.value ~default:Amount.zero
        |> Amount.to_n |> N.to_z |> Z.to_int
      in
      let wrap_error t =
        t |> Result.map_error (fun x -> `Origination_error (x, sender))
      in
      let%ok contract_storage, ledger, contract_address =
        Ledger.with_ticket_table ledger (fun ~get_table ~set_table ->
            let table = get_table () in
            let ticket_ids = tickets |> List.map fst in
            let%ok ticket_ids', table =
              Ticket_table.take_tickets table ~sender ~ticket_ids
            in

            let contract_address =
              Contract_address.of_user_operation_hash operation_hash
            in
            let address = Address.of_contract_hash contract_address in

            let table =
              Ticket_table.update_tickets table ~sender:address
                ~ticket_ids:ticket_ids'
            in
            (* TODO: Burn on storage size change, need CTEZ *)
            let%ok contract =
              Contract_vm.Compiler.compile
                ~tickets:
                  (List.map (fun (f, r) -> (f, fst r)) tickets |> List.to_seq)
                ~gas:initial_gas payload ~sender
              |> wrap_error
            in
            let contract_storage =
              Contract_storage.originate_contract contract_storage
                ~address:contract_address ~contract
            in
            let ledger = set_table table in
            Ok (contract_storage, ledger, contract_address))
        |> Result.map_error (function
             | `Insufficient_funds ->
                 `Origination_error ("not_enough_funds", sender)
             | x -> x)
      in
      let tickets = Stdlib.List.map fst tickets in
      Ok
        ( (ledger, contract_storage),
          Receipt.contract_originated ~operation:operation_hash
            ~contract_address ~tickets )
  | Contract_invocation { to_invoke; argument; tickets } ->
      let balance = Core.Int.max_value |> amount_of_int_or_zero in
      (* TODO: find good transaction cost *)
      let invocation_cost = 250 |> amount_of_int_or_zero in
      let%assert () =
        Amount.
          ( `Invocation_error ("Not enought funds", sender),
            let comparison_result = compare balance invocation_cost in
            comparison_result >= 0 )
      in
      let burn_cap = invocation_cost in
      let initial_gas =
        Amount.(balance - burn_cap)
        |> Stdlib.Option.value ~default:Amount.zero
        |> Amount.to_n |> N.to_z |> Z.to_int
      in
      let wrap_error t =
        Result.map_error (fun x -> `Invocation_error (x, sender)) t
      in
      let%ok contract =
        Contract_storage.get_contract contract_storage ~address:to_invoke
        |> Option.to_result ~none:"Contract not found"
        |> wrap_error
      in
      Ledger.with_ticket_table ledger (fun ~get_table ~set_table ->
          let table = get_table () in
          let table_tickets, table =
            Ticket_table.take_all_tickets table
              ~sender:(Address.of_contract_hash to_invoke)
          in
          let ticket_ids = List.map fst tickets in
          let%ok _, table =
            Ticket_table.take_tickets table ~sender ~ticket_ids
          in
          let ctx, to_replace =
            Context.make_state ~sender ~source:sender
              ~mapping:(Contract_vm.Contract.tickets_mapping contract)
              ~contract_owned_tickets:table_tickets
              ~self:(Address.of_contract_hash to_invoke)
              ~provided_tickets:(Stdlib.List.to_seq tickets)
              ~get_contract_opt:(fun address ->
                let address = Address.to_contract_hash address |> Option.get in
                Contract_storage.get_contract contract_storage ~address
                |> Option.map (Core.Fn.const (Address.of_contract_hash address)))
          in
          let%ok contract, user_op_list =
            Contract_vm.Interpreter.invoke ~to_replace ~ctx ~arg:argument
              ~gas:initial_gas contract
            |> wrap_error
          in

          let%ok tickets_mapping, tickets, operations =
            ctx#finalize user_op_list
            |> Result.map_error (fun `Execution_error ->
                   `Invocation_error ("execution error", sender))
          in
          let contract =
            Contract_vm.Contract.update_tickets contract tickets_mapping
          in
          let contract_storage =
            Contract_storage.update_contract_storage contract_storage
              ~address:to_invoke ~updated_contract:contract
          in
          let table =
            Ticket_table.update_tickets table
              ~sender:(Address.of_contract_hash to_invoke)
              ~ticket_ids:tickets
          in
          let ledger = set_table table in

          let init =
            ( (ledger, contract_storage),
              Receipt.success ~operation:operation_hash )
          in

          Stdlib.List.fold_left
            (fun acc contract_op ->
              let (ledger, contract_storage), receipt = acc in
              let result =
                apply_contract_op ~source:sender ~sender ~level ~nonce
                  ~apply_operation_content contract_storage ledger contract_op
              in
              match result with
              | Ok (state, receipt2) -> (state, Receipt.chain receipt receipt2)
              | Error error ->
                  let next_receipt =
                    Receipt.error ~operation:operation_hash ~error
                  in
                  ( (ledger, contract_storage),
                    Receipt.chain receipt next_receipt ))
            init operations
          |> Result.ok)

and apply_contract_op ~source ~sender ~level ~nonce ~apply_operation_content
    contract_storage ledger contract_op =
  match contract_op with
  | Context.Operation.Invoke { tickets; destination; param } ->
      let%ok destination =
        Address.to_contract_hash destination
        |> Option.to_result
             ~none:
               (`Invalid_address_error
                 (Format.sprintf
                    "invalid address.\nsender: %s\n destination: %s"
                    (Address.to_b58 sender)
                    (Address.to_b58 destination)))
      in
      let%ok payload =
        Contract_vm.Invocation_payload.of_bytes ~arg:param
        |> Result.map_error (fun x ->
               `Invalid_payload (Format.sprintf "invalid_payload %s" x))
      in
      let contract_operation =
        Contract_operation.Contract_invocation
          { to_invoke = destination; argument = payload; tickets }
      in
      let operation_content, operation_hash =
        Operation.content_of_contract_operation ~contract_operation ~level
          ~nonce ~source
      in
      apply_operation_content ~operation_hash ~source ~level ~nonce
        contract_storage ledger operation_content
      |> Result.ok
  | Transfer { ticket; amount; destination } ->
      let%ok destination =
        Address.to_key_hash destination
        |> Option.to_result
             ~none:
               (`Invalid_address_error
                 (Format.sprintf
                    "invalid implicit address.\nsender: %s\n destination: %s"
                    (Address.to_b58 sender)
                    (Address.to_b58 destination)))
      in
      let receiver = Address.of_key_hash destination in
      let operation_content, operation_hash =
        Operation.make_transaction_content ~level ~nonce ~source ~receiver
          ~amount ~ticket_id:ticket
      in
      apply_operation_content ~operation_hash ~source ~level ~nonce
        contract_storage ledger operation_content
      |> Result.ok

let apply ~sender ~operation_hash ~level ~nonce ~apply_operation_content ledger
    contract_storage operation =
  let result =
    apply ~sender ~operation_hash ~level ~nonce ~apply_operation_content ledger
      contract_storage operation
  in
  match result with
  | Ok (state, receipt) -> (state, receipt)
  | Error error ->
      ( (ledger, contract_storage),
        Receipt.error ~operation:operation_hash ~error )
