(* Yes, *_helpers is a naming antipattern. PR's accepted. *)
open Helpers
open Feather
open Crypto
open Tezos

let run_res ?(error = "") cmd =
  match collect stdout_and_status cmd with
  | stdout, 0 -> Ok stdout
  | stdout, _ -> Error (error ^ stdout)

type mode =
  | Docker
  | Local

let mode_to_string mode =
  match mode with
  | Docker -> "docker"
  | Local -> "local"

let mode_of_string = function
  | "docker" -> Ok Docker
  | "local" -> Ok Local
  | _ -> Error "The allowed mode is docker or local"

(* helpers *)
let deku_node args = process "deku-node" args

let deku_cli args = process "deku-cli" args

let rm_dir directory = process "rm" ["-rf"; directory] |> run

let ligo args = process "ligo" args

let tezos_client args =
  process "docker"
    (List.append ["exec"; "-t"; "deku_flextesa"; "tezos-client"] args)
  |> run_res ~error:"error in tezos-client"

let rpc_url mode =
  match mode with
  | Docker -> "http://flextesa:20000"
  | Local -> "http://localhost:20000"

let get_contract_address rpc_url contract_name =
  let%ok stdout =
    tezos_client
      ["--endpoint"; rpc_url; "show"; "known"; "contract"; contract_name] in
  stdout
  |> String.split_on_char '\n'
  |> List.filter (String.starts_with ~prefix:"KT1")
  |> (fun list -> List.nth_opt list 0)
  |> Option.map String.trim
  |> Option.map Address.of_string
  |> Option.join
  |> Option.to_result ~none:"Error in contract retrieving"

let deploy_contract rpc_url contract_name contract_path storage wallet =
  Format.printf "Originating new %s contract." contract_name;
  let%ok storage =
    ligo ["compile"; "storage"; contract_path; storage]
    |> run_res ~error:"ligo compile storage error" in
  let%ok contract =
    ligo ["compile"; "contract"; contract_path]
    |> run_res ~error:"ligo compile contract error" in
  let%ok _ =
    tezos_client
      [
        "--endpoint";
        rpc_url;
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
  get_contract_address rpc_url contract_name

let deku_address =
  "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Key_hash.of_string |> Option.get

let deku_secret =
  "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
  |> Secret.of_string
  |> Option.get
