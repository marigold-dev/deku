open Deku_tezos
open Cmdliner
open Arg

let tezos_address =
  let parser string =
    string |> Address.of_string
    |> Option.to_result ~none:(`Msg "Cannot parse the given address")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Address.to_string)
  in
  conv (parser, printer)

let deku_address =
  let parser string =
    string |> Deku_ledger.Address.of_b58
    |> Option.to_result ~none:(`Msg "Cannot parse the given address")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_ledger.Address.to_b58)
  in
  conv (parser, printer)

let secret =
  let parser string =
    string |> Deku_crypto.Secret.of_b58
    |> Option.to_result ~none:(`Msg "Cannot parse the given secret key")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_crypto.Secret.to_b58)
  in
  conv (parser, printer)

let ticket_id =
  let string_of_error = function
    | `Ticket_from_implicit -> `Msg "Cannot parse the given contract address"
    | `Cannot_parse -> `Msg "Expected a ticket id"
  in
  let parser string =
    Deku_ledger.Ticket_id.of_string string |> Result.map_error string_of_error
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_ledger.Ticket_id.to_string)
  in
  conv (parser, printer)

let operation_hash =
  let parser string =
    Deku_protocol.Operation_hash.of_b58 string
    |> Option.to_result ~none:(`Msg "Cannot parse operation hash")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (Deku_protocol.Operation_hash.to_b58 address)
  in
  conv (parser, printer)

let uri =
  let parser uri = Ok (uri |> Uri.of_string) in
  let printer ppf uri = Format.fprintf ppf "%s" (uri |> Uri.to_string) in
  let open Arg in
  conv (parser, printer)

let consensus_contract =
  let docv = "consensus-contract" in
  let doc = "The address of the consensus contract to listen." in
  let env = Cmd.Env.info "DEKU_CONSENSUS_CONTRACT" in
  required
  & opt (some tezos_address) None
  & info [ "consensus-contract" ] ~doc ~docv ~env

let tezos_address position =
  let docv = "tezos-address" in
  let doc = "A tezos address" in
  required & pos position (some tezos_address) None & info [] ~doc ~docv

let ticket_id position =
  let docv = "ticket-id" in
  let doc = "The ticket id to use" in
  required & pos position (some ticket_id) None & info [] ~doc ~docv

let deku_address position =
  let docv = "deku-address" in
  let doc = "A Deku address" in
  required & pos position (some deku_address) None & info [] ~doc ~docv

let secret position =
  let docv = "secret" in
  let doc = "A secret key" in
  required & pos position (some secret) None & info [] ~doc ~docv

let operation_hash position =
  let docv = "operation-hash" in
  let doc = "An operation hash" in
  required & pos position (some operation_hash) None & info [] ~doc ~docv

let verbose_test =
  let docv = "verbose-test" in
  let doc = "Verbose tests? Default is no" in
  let default = false in
  let env = Cmd.Env.info "DEKU_VERBOSE_TESTS" in
  value & opt bool default & info [ "verbose-test" ] ~doc ~docv ~env

let host =
  let docv = "api-node" in
  let doc = "Deku API hostname (default http://localhost:8080)" in
  let default = Uri.of_string "http://localhost:8080" in
  let env = Cmd.Env.info "DEKU_API_NODE" in
  value & opt uri default & info [ "api-node" ] ~doc ~docv ~env

(* Helpers to post/get json on the network*)
(* TODO: copy pasted from repr.ml of the API*)
module Operation_dto = struct
  open Deku_crypto
  open Deku_protocol

  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { key; signature; initial } -> (key, signature, initial))
      (fun (key, signature, initial) -> { key; signature; initial })
      (obj3 (req "key" Key.encoding)
         (req "signature" Signature.encoding)
         (req "initial" Operation.Initial.encoding))

  let of_signed signed =
    let (Operation.Signed.Signed_operation { key; signature; initial }) =
      signed
    in
    { key; signature; initial }

  let to_signed repr =
    let { key; signature; initial } = repr in
    Operation.Signed.make_with_signature ~key ~signature ~initial
end

module Net = struct
  let post_operation ~sw ~env operation uri =
    let ope_dto = Operation_dto.of_signed operation in
    let json = Data_encoding.Json.construct Operation_dto.encoding ope_dto in
    let body = Data_encoding.Json.to_string json in
    let body = Piaf.Body.of_string body in
    match Piaf.Client.Oneshot.post ~body env uri ~sw with
    | Ok response -> response
    | Error error -> failwith (Piaf.Error.to_string error)

  let get ~sw ~env uri =
    match Piaf.Client.Oneshot.get env uri ~sw with
    | Ok response -> response
    | Error error -> failwith (Piaf.Error.to_string error)

  let body_of_response (response : Piaf.Response.t) =
    match Piaf.Body.to_string response.body with
    | Ok body -> body
    | Error error -> failwith (Piaf.Error.to_string error)

  let code_of_response (response : Piaf.Response.t) =
    response.Piaf.Response.status |> Piaf.Status.to_code

  let level_of_response json =
    let open Deku_concepts in
    let level_response_encoding =
      let open Data_encoding in
      obj1 (req "level" Level.encoding)
    in
    match Data_encoding.Json.from_string json with
    | Ok json -> (
        try Data_encoding.Json.destruct level_response_encoding json
        with _ -> failwith "Cannot parse the body from level endpoint")
    | _ -> failwith "Wrong body received from level endpoint"
end

(* *)
