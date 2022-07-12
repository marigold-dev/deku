open Helpers

exception Invocation_error

let invoke ~ctx ~storage ~argument ~sender code =
  let open Core_deku in
  let open Contracts in
  match
    let argument = [%to_yojson: bytes] argument in
    let code = Bytes.of_string code in
    let gas = Int.max_int in
    let%ok payload =
      Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage in
    let%ok modd =
      Contract_vm.Compiler.compile payload ~tickets:Seq.empty ~gas ~sender in
    let%ok argument =
      Contract_vm.Invocation_payload.wasm_of_yojson ~arg:argument in
    let%ok contract, ops =
      Contract_vm.Interpreter.invoke ~to_replace:None ~ctx ~arg:argument ~gas
        modd in
    Ok (Contract_vm.Contract.raw_storage contract, ops)
  with
  | Ok storage -> storage
  | Error _ -> raise Invocation_error
  | exception _ -> raise Invocation_error

let i32 t =
  let b = Bytes.make 4 '0' in
  let () = Bytes.set_int32_le b 0 t in
  b

let i64 t =
  let b = Bytes.make 8 '0' in
  let () = Bytes.set_int64_le b 0 t in
  b

let make_ticket ?ticketer ?data () =
  let open Crypto in
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Random.generate 20
        |> Cstruct.to_string
        |> BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  Core_deku.Ticket_id.of_tezos { ticketer; data } |> Result.get_ok

let make_address () =
  let open Crypto in
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  Core_deku.Address.of_key_hash key_hash

let make_contract_address str : Core_deku.Contract_address.t =
  Crypto.BLAKE2B_20.hash str |> Obj.magic

module Testables = struct
  open Core_deku
  open Contracts

  let amount =
    Alcotest.of_pp (fun fmt x ->
        Format.fprintf fmt "%d" (Context.Amount.to_int x))

  let ticket_id =
    Alcotest.of_pp (fun fmt p ->
        Format.fprintf fmt "%s" (Core_deku.Ticket_id.to_string p))

  let contract_operation =
    let open Core_deku in
    let open Contracts in
    Alcotest.of_pp (fun fmt -> function
      | Context.Operation.Transfer x ->
        Format.fprintf fmt "amount: %d\n destination: %S\n"
          (Amount.to_int x.amount)
          (Address.to_string x.destination)
      | Invoke { destination; param; tickets } ->
        let format_tickets fmt t =
          Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" "\nNext\n")
            (fun fmt ((ticket_id, amount), (handle, ptr)) ->
              Format.fprintf fmt
                "ticket_id: %s\namount: %d\nhandle: %s\noffset: %a"
                (Ticket_id.to_string ticket_id)
                (Amount.to_int amount) (Int32.to_string handle)
                (Format.pp_print_option (fun fmt x ->
                     Format.fprintf fmt "%s" (Int64.to_string x)))
                ptr)
            fmt t in
        Format.fprintf fmt "\ndestination: %s\nparam: %S\ntickets:@[%a@]\n"
          (Address.to_string destination)
          (Core.Bytes.Hexdump.to_string_hum param)
          format_tickets tickets)
end

let make_custom ~tickets_table ~source ~sender ~self ~tickets =
  let open Core_deku in
  let open Contracts in
  let ctx =
    Context.make_state ~source ~sender ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self ~provided_tickets:tickets in
  ctx
