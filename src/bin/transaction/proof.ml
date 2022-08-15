open Deku_protocol
open Deku_crypto
open Cohttp_lwt_unix

let post body uri =
  let body = Cohttp_lwt.Body.of_string body in
  Client.post ~body uri

module Proof_response = struct
  type t =
    | Proof of {
        hash : BLAKE2b.t;
        handle : Ledger.Withdrawal_handle.t;
        proof : Ledger.withdraw_proof;
      }
  [@@deriving yojson]
end

let () =
  let arg1 = Sys.argv.(1) in
  let arg1 = Printf.sprintf "\"%s\"" arg1 in
  let arg1 = Yojson.Safe.from_string arg1 in
  let operation_hash = Operation_hash.t_of_yojson arg1 in
  let body = Operation_hash.yojson_of_t operation_hash in
  let body = Yojson.Safe.to_string body in
  Lwt_main.run
    (let open Lwt.Syntax in
    let* _reponse, body =
      post body (Uri.of_string "http://localhost:4440/withdraw_proof")
    in
    let* body = Cohttp_lwt.Body.to_string body in
    prerr_endline body;
    let body = Yojson.Safe.from_string body in
    match Proof_response.t_of_yojson body with
    | Proof_response.Proof
        {
          handle = { id; owner; ticket_id; amount; hash = handle_hash };
          proof;
          hash = _;
        } ->
        let to_hex bytes = Hex.show (Hex.of_bytes bytes) in
        let (Ticket_id { ticketer; data }) = ticket_id in
        Format.printf
          {|(Pair (Pair %S
            (Pair (Pair %s 0x%s) %d %S)
            %S)
      0x%s
      { %s })|}
          "fixme_callback"
          (Deku_concepts.Amount.show amount)
          (to_hex data) id
          (Deku_tezos.Address.to_string owner)
          (Deku_tezos.Contract_hash.to_string ticketer)
          (BLAKE2b.to_hex handle_hash)
          (List.map
             (fun (left, right) ->
               Format.sprintf "        Pair 0x%s\n             0x%s"
                 (BLAKE2b.to_hex left) (BLAKE2b.to_hex right))
             proof
          |> String.concat " ;\n" |> String.trim);
        Lwt.return_unit)
