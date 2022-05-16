open Node
open Helpers
open Bin_common

open (
  struct
    include Server
  end :
    sig end)

let interop_context node_folder =
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

(*let get_random_validator_uri validators_uris () =
  (* TODO: use Random *)
  List.nth validators_uris 0 |> Uri.of_string*)

(*let get_current_block_level node_folder () =
  let open Network in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris -> (
    let validator_uris = List.filter_map (function
    | key_hash, Some uri -> Some (key_hash, uri)
    | _ -> None)
    validator_uris
    in
    match validator_uris with
    | [] -> Lwt.return (`Error (false, "No validators found"))
    | (_, validator_uri) :: _ ->
     let%await block_level = Network.request_block_level () validator_uri in
     let block_level = block_level.level in
     Lwt.return block_level
    ) *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let get_random_validator_uri () = List.nth validators_uris 0 |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

let spam_transactions ~ticketer ~n () = Lwt.return ""

let rec transfers_spam_tickets ~ticketer =
  let n = 2000 in
  let%await _ =
    (* Fun.id: is the identity function. For any argument x, id x is x *)
    Lwt_list.iter_p Fun.id
    @@ (* List.init len f:
          let make_list n = List.init n ~f:(fun x -> x)
          create lists using List.init, which takes an integer n and a function f,
          and create a list of length n, where the data for each element is create by
          calling f on the index of that element.
           List.init 5 ~f:(Fn.id);;
              - : int list = [0; 1; 2; 3; 4]
          making a list of spam_transactions of length 4,
          each time transfers this ticketer n:2000 times
       *)
    List.init 4 (fun _ ->
        let%await _ = spam_transactions ~ticketer ~n () in
        await ()) in
  (* sleep d: is a promise that remains in a pending state for d
     seconds and after that it is resolved with value ().
  *)
  let%await () = Lwt_unix.sleep 1.0 in
  transfers_spam_tickets ~ticketer

let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  transfers_spam_tickets ~ticketer

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run
