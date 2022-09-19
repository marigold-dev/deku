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
    string |> Deku_protocol.Address.of_b58
    |> Option.to_result ~none:(`Msg "Cannot parse the given address")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_protocol.Address.to_b58)
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
    Deku_protocol.Ticket_id.of_string string |> Result.map_error string_of_error
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_protocol.Ticket_id.to_string)
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

let consensus_contract =
  let docv = "consensus-contract" in
  let doc = "The address of the consensus contract to listen." in
  let env = Cmd.Env.info "DEKU_CONSENSUS_CONTRACT" in
  required & opt (some tezos_address) None & info ["consensus-contract"] ~doc ~docv ~env

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
