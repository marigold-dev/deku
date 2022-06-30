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

let curl args = process "curl" (["--silent"] @ args)

let tezos_client ?(wait = None) args =
  let wait =
    match wait with
    | None -> "none"
    | Some n -> string_of_int n in
  process "docker"
    (List.append
       ["exec"; "-t"; "deku_flextesa"; "tezos-client"; "--wait"; wait]
       args)
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

let deploy_contract ?(wait = None) rpc_url contract_name contract_path storage
    wallet =
  Format.printf "Originating new %s contract.@." contract_name;
  let%ok storage =
    ligo ["compile"; "storage"; contract_path; storage]
    |> run_res ~error:"ligo compile storage error" in
  let%ok contract =
    ligo ["compile"; "contract"; contract_path]
    |> run_res ~error:"ligo compile contract error" in
  let%ok _ =
    tezos_client ~wait
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

let make_validators nodes = List.init nodes (fun i -> i)

(** Try to execute a given function n time, with a spacing time of 1 seconds between each call, verify if the result match the predicate function **)
let rec retry ?(tries = 60) f verify =
  (* FIXME: I think we should get rid of the verify param *)
  if tries <= 0 then
    Error "function did not succeed"
  else
    let res = f () |> Result.fold ~ok:verify ~error:(fun error -> Error error) in
    match res with
    | Ok res -> Ok res
    | Error _ ->
      Unix.sleep 1;
      retry f ~tries:(tries - 1) verify

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

module Cmdliner_helpers = struct
  open Cmdliner

  let exits =
    Cmd.Exit.defaults
    @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

  let man =
    [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

  (* parsing *)
  let mode =
    let printer fmt mode = Format.fprintf fmt "%s" (mode_to_string mode) in
    let open Arg in
    let docv = "mode" in
    let doc = "The mode of the cluster, it can be docker or local" in
    let mode_parser = conv' ~docv (mode_of_string, printer) in
    value & opt mode_parser Local & info ["mode"] ~docv ~doc

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
end
