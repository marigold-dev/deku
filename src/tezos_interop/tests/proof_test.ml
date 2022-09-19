(* Called by a script to request a withdraw proof and print it to stdout *)
(* TODO: check that proofs are valid on Tezos *)
(* TODO: if we keep these tests, refactor with withdraw_test *)

open Deku_protocol
open Deku_crypto
open Cohttp_lwt_unix

let post body uri =
  let body = Cohttp_lwt.Body.of_string body in
  Client.post ~body uri

let main operation_hash verbose =
  try
    let body = Operation_hash.yojson_of_t operation_hash in
    let body = Yojson.Safe.to_string body in
    Lwt_main.run
      (let open Lwt.Syntax in
      let* _reponse, body =
        post body (Uri.of_string "http://localhost:4440/withdraw_proof")
      in
      let* body = Cohttp_lwt.Body.to_string body in
      if verbose then prerr_endline body;
      let body = Yojson.Safe.from_string body in
      match Ledger.Proof_response.t_of_yojson body with
      | Ledger.Proof_response.Proof
          {
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
