(* Called by a script to request a withdraw proof and print it to stdout *)
(* TODO: check that proofs are valid on Tezos *)
(* TODO: if we keep these tests, refactor with withdraw_test *)

open Deku_protocol
open Deku_crypto
open Common
open Deku_ledger

type api_response = {
  withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
  handle : Ledger.Withdrawal_handle.t;
  proof : Ledger.withdraw_proof;
}

let encoding =
  let open Data_encoding in
  let open Deku_ledger.Ledger in
  conv
    (fun { withdrawal_handles_hash; handle; proof } ->
      (withdrawal_handles_hash, handle, proof))
    (fun (withdrawal_handles_hash, handle, proof) ->
      { withdrawal_handles_hash; handle; proof })
    (obj3
       (req "withdrawal_handles_hash"
          Withdrawal_handle.Withdrawal_handle_hash.encoding)
       (req "handle" Withdrawal_handle.encoding)
       (req "proof" withdraw_proof_encoding))

let main operation_hash verbose host =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let body = Operation_hash.to_b58 operation_hash in
  let url = Uri.with_path host ("api/v1/proof/" ^ body) in
  let response = Net.get ~sw ~env url in
  let code = Net.code_of_response response in
  let body = Net.body_of_response response in
  if verbose then Printf.eprintf "[%d]    %s\n%!" code body;
  let body =
    match body |> Data_encoding.Json.from_string with
    | Ok body -> body
    | Error _ -> failwith "cannot parse body"
  in
  let {
    handle = { id; owner; ticket_id; amount; hash = _handle_hash };
    proof;
    withdrawal_handles_hash;
  } =
    Data_encoding.Json.destruct encoding body
  in
  let to_hex bytes = Hex.show (Hex.of_bytes bytes) in
  (* FIXME: we should remove -8 in the future. *)
  let[@warning "-8"] (Ticket_id.Ticket_id { ticketer = Tezos ticketer; data }) =
    ticket_id
  in
  Format.printf
    {|(Pair (Pair %S
      (Pair (Pair %s 0x%s) %d %S)
      %S)
      0x%s
      { %s })|}
    "burn_callback"
    (Deku_concepts.Amount.show amount)
    (to_hex data) id
    (Deku_tezos.Contract_hash.to_b58 ticketer)
    (Deku_tezos.Address.to_string owner)
    (BLAKE2b.to_hex withdrawal_handles_hash)
    (List.map
       (fun (left, right) ->
         Format.sprintf "        Pair 0x%s\n             0x%s"
           (BLAKE2b.to_hex left) (BLAKE2b.to_hex right))
       proof
    |> String.concat " ;\n" |> String.trim);
  print_endline "";
  exit 0

open Cmdliner

let info =
  let doc = "Get a withdraw proof from an operation hash" in
  Cmd.info "deku-proof-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.operation_hash 0 $ Common.verbose_test $ Common.host

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
