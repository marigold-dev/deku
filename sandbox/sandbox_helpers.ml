(* Yes, *_helpers is a naming antipattern. PR's accepted. *)
open Helpers
open Feather
open Crypto
open Tezos

let run_res ?(error = "") cmd =
  match collect stdout_and_status cmd with
  | stdout, 0 -> Ok stdout
  | stdout, _ -> Error (error ^ stdout)

let deku_node args = process "deku-node" args

let deku_cli args = process "deku-cli" args

let rm_dir directory = process "rm" ["-rf"; directory] |> run

let ligo args = process "ligo" args

let curl args = process "curl" (["--silent"] @ args)

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

let deploy_contract ?(wait = None) rpc_address contract_name contract_path
    storage wallet =
  Format.printf "Originating new %s contract.@." contract_name;
  let%ok storage =
    ligo ["compile"; "storage"; contract_path; storage]
    |> run_res ~error:"ligo compile storage error" in
  let%ok contract =
    ligo ["compile"; "contract"; contract_path]
    |> run_res ~error:"ligo compile contract error" in
  print_endline "deploying contract";
  Format.printf "rpc_address: %s\n%!" (Uri.to_string rpc_address);
  let%ok _ =
    tezos_client ~wait
      [
        "--endpoint";
        Uri.to_string rpc_address;
        "originate";
        "contract";
        contract_name;
        "transferring";
        "0";
        "from";
        wallet;
        "running";
        contract;
        "--init";
        storage;
        "--burn-cap";
        "2";
        "--force";
      ] in
  print_endline "delpoyed contract";
  get_contract_address rpc_address contract_name

let deku_address =
  "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Key_hash.of_string |> Option.get

let deku_secret =
  "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
  |> Secret.of_string
  |> Option.get

let make_validators nodes = List.init nodes (fun i -> i)

(** Try to execute a given function n time, with a spacing time of 1 seconds between each call **)
let rec retry ?(tries = 60) f =
  if tries <= 0 then
    Error "function did not succeed"
  else
    match f () with
    | Ok res -> Ok res
    | Error _ ->
      Unix.sleep 1;
      retry ~tries:(tries - 1) f

let known_handles_hash_big_map_id consensus_address =
  let%ok stdout =
    tezos_client
      ["get"; "contract"; "storage"; "for"; Address.to_string consensus_address]
    |> Result.map (Str.global_replace (Str.regexp "\n") "") in
  let regex = Str.regexp ".*(Pair\\( [0-9]+\\) [0-9]+).*" in
  match Str.string_match regex stdout 0 with
  | false -> Error "cannot retrieve the big map id"
  | true -> Str.matched_group 1 stdout |> String.trim |> int_of_string |> ok

let get_big_map_size big_map_id =
  process "curl"
    [
      "--silent";
      Format.sprintf
        "http://localhost:20000/chains/main/blocks/head/context/raw/json/big_maps/index/%i/total_bytes"
        big_map_id;
    ]
  |> run_res

let is_node_bootstrapped rpc_url =
  tezos_client ["--endpoint"; rpc_url; "bootstrapped"]
  |> Result.map (String.split_on_char '\n')
  |> Result.map List.rev
  |> Result.map (fun list -> List.nth_opt list 0)
  |> Result.map
       (Option.to_result ~none:"No output from tezos-client bootstrapped")
  |> Result.join
  |> Result.map (String.starts_with ~prefix:"Node is bootstrapped")
  |> Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false)

let tezos_client_update_config rpc_url =
  tezos_client ["--endpoint"; rpc_url; "config"; "update"]

let import_secret rpc_url alias secret =
  tezos_client
    ["--endpoint"; rpc_url; "import"; "secret"; "key"; alias; secret; "--force"]

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
    & opt string "edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"
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
