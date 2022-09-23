(* Called by a script to request a withdraw proof and print it to stdout *)
(* TODO: check that proofs are valid on Tezos *)
(* TODO: if we keep these tests, refactor with withdraw_test *)

open Deku_protocol
open Deku_crypto
open Cohttp_lwt_unix

let get uri = Client.get uri

type api_response = {
  withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
  handle : Ledger.Withdrawal_handle.t;
  proof : Ledger.withdraw_proof;
}
[@@deriving of_yojson]

let main operation_hash verbose =
  try
    let body = Operation_hash.to_b58 operation_hash in
    Lwt_main.run
      (let open Lwt.Syntax in
      let* _reponse, body =
        get (Uri.of_string ("http://localhost:8080/api/v1/proof/" ^ body))
      in
      let* body = Cohttp_lwt.Body.to_string body in
      if verbose then prerr_endline body;
      let body = Yojson.Safe.from_string body in
      match api_response_of_yojson body with
      | {
       handle = { id; owner; ticket_id; amount; hash = _handle_hash };
       proof;
       withdrawal_handles_hash;
      } ->
          let to_hex bytes = Hex.show (Hex.of_bytes bytes) in
          let (Ticket_id { ticketer; data }) = ticket_id in
          Format.printf
            {|(Pair (Pair %S
              (Pair (Pair %s 0x%s) %d %S)
              %S)
        0x%s
        { %s })|}
            "burn_callback"
            (Deku_concepts.Amount.show amount)
            (to_hex data) id
            (Deku_tezos.Contract_hash.to_string ticketer)
            (Deku_tezos.Address.to_string owner)
            (BLAKE2b.to_hex withdrawal_handles_hash)
            (List.map
               (fun (left, right) ->
                 Format.sprintf "        Pair 0x%s\n             0x%s"
                   (BLAKE2b.to_hex left) (BLAKE2b.to_hex right))
               proof
            |> String.concat " ;\n" |> String.trim);
          print_endline "";
          Lwt.return_unit)
  with e -> if verbose then raise e

open Cmdliner

let info =
  let doc = "Get a withdraw proof from an operation hash" in
  Cmd.info "deku-proof-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.operation_hash 0 $ Common.verbose_test

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
