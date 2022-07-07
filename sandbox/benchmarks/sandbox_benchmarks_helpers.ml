(* Yes, *_helpers is a naming antipattern. PR's accepted. *)
open Helpers
open Feather
open Tezos

let run_res ?(error = "") cmd =
  match collect stdout_and_status cmd with
  | stdout, 0 -> Ok stdout
  | stdout, _ -> Error (error ^ stdout)

let tezos_client ?(wait = None) args =
  let wait =
    match wait with
    | None -> []
    | Some n -> ["--wait"; string_of_int n] in
  process "nix"
    (["run"; "github:marigold-dev/tezos-nix#tezos-client"; "--"] @ wait @ args)
  |> run_res ~error:"error in tezos-client"

let get_contract_address rpc_address contract_name =
  let%ok stdout =
    tezos_client
      [
        "--endpoint";
        Uri.to_string rpc_address;
        "show";
        "known";
        "contract";
        contract_name;
      ] in
  stdout
  |> String.split_on_char '\n'
  |> List.filter (String.starts_with ~prefix:"KT1")
  |> (fun list -> List.nth_opt list 0)
  |> Option.map String.trim
  |> Option.map Address.of_string
  |> Option.join
  |> Option.to_result ~none:"Error in contract retrieving"

module Cmdliner_helpers = struct
  open Cmdliner

  let exits =
    Cmd.Exit.defaults
    @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

  let man =
    [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

  (* TODO: copied from deku_cli *)
  let uri =
    let parser uri = Ok (uri |> Uri.of_string) in
    let printer ppf uri = Format.fprintf ppf "%s" (uri |> Uri.to_string) in
    let open Arg in
    conv (parser, printer)

  let rpc_address =
    let open Arg in
    let doc = "rpc_address" in
    let docv =
      "The address of the Tezos RPC server used to communicate with the main \
       chain" in
    let env = Cmd.Env.info "DEKU_RPC_ADDRESS" in
    value
    & opt uri (Uri.of_string "http://localhost:20000")
    & info ["rpc_address"] ~docv ~doc ~env

  let tezos_secret =
    let open Arg in
    let doc = "tezos_secret" in
    let docv =
      "The secret key used to deploy the contract to Tezos. Defaults the \
       secret corresponding to one of the bootstrapping accounts used in \
       Flextesa" in
    let env = Cmd.Env.info "DEKU_TEZOS_SECRET" in
    value
    & opt string "edsk4TxW4UvCXFZrB5ifMx83PAUECKLUB95ecm1Lp4GbE8ZEeE3T1g"
    & info ["tezos_secret"] ~docv ~doc ~env

  let nodes =
    let parser string =
      let%ok nodes =
        int_of_string_opt string
        |> Option.to_result ~none:(`Msg "number of nodes has to be a number.")
      in
      let%assert () =
        (`Msg "number of nodes must be a strictly positive number", nodes > 0)
      in
      Ok nodes in
    let printer fmt nodes = Format.fprintf fmt "%i" nodes in
    let open Arg in
    let nodes_parser = conv ~docv:"nodes" (parser, printer) in
    let docv = "nodes" in
    let doc = "The number of nodes you want in your cluster" in
    value & opt nodes_parser 3 & info ["nodes"] ~docv ~doc

  (* convenient way to add commands *)
  module Cli = struct
    module type COMMAND = sig
      val term : (unit, string) result Term.t

      val info : Cmd.info
    end

    type 'a t = {
      info : Cmd.info;
      commands : 'a Cmd.t list;
    }

    let make ~info () = { info; commands = [] }

    let add (module Command : COMMAND) t =
      {
        info = t.info;
        commands = Cmd.v Command.info Command.term :: t.commands;
      }

    let eval t = exit @@ Cmd.eval_result @@ Cmd.group t.info t.commands
  end
end
