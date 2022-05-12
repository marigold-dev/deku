open Helpers

exception Invocation_error

let invoke ?(custom = fun _ _ -> ()) ~storage ~argument code =
  match
    let%ok contract = Wasm_vm.Module.of_string ~code in
    let gas = ref 10000 in
    Wasm_vm.Runtime.invoke custom ~module_:contract ~gas ~argument ~storage
  with
  | Ok storage -> storage
  | Error e ->
    print_endline (Wasm_vm.Errors.show e);
    raise Invocation_error

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
  { ticketer; data }

let make_address () =
  let open Crypto in
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  Core_deku.Address.of_key_hash key_hash

let make_contract_address str : Core_deku.Contract_address.t =
  Crypto.BLAKE2B_20.hash str |> Obj.magic

module Testables = struct
  open Core_deku

  let ticket_handle =
    Alcotest.of_pp (fun fmt x ->
        Format.fprintf fmt "%s" (Ticket_handle.to_string x))

  let contract_operation =
    let open Contract_operation in
    Alcotest.of_pp (fun fmt -> function
      | Transfer x ->
        Format.fprintf fmt "amount: %d\n destination: %s\n"
          (Amount.to_int x.amount)
          (Address.to_string x.destination)
      | Invoke x ->
        Format.fprintf fmt "param: %s\n  destination: %s\n"
          (Bytes.to_string x.param)
          (Address.to_string x.destination))
end

module CTX = Wasm_vm.Ffi.Make (Core_deku.Contract_context.CTX)

let make_custom ~tickets_table ~source ~sender ~self ~tickets =
  let ctx =
    Core_deku.Contract_context.make_state ~source ~sender
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self ~provided_tickets:tickets in
  CTX.custom ~ctx
