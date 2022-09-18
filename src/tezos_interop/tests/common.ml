open Cmdliner
open Arg

let tezos_address =
  let parser string =
    string |> Deku_tezos.Address.of_string
    |> Option.to_result ~none:(`Msg "Cannot parse the given address")
  in
  let printer ppf address =
    Format.fprintf ppf "%s" (address |> Deku_tezos.Address.to_string)
  in
  conv (parser, printer)

let consensus_contract =
  let docv = "consensus-contract" in
  let doc = "The address of the consensus contract to listen." in
  let env = Cmd.Env.info "DEKU_CONSENSUS_CONTRACT" in
  required & opt (some tezos_address) None & info ["consensus-contract"] ~doc ~docv ~env

