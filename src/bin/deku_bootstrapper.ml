open Cmdliner
open Helpers
open Network

(* TODO: creates bin/common.ml, this code appears in three different places, deku_cli, deku_node, here *)
let lwt_ret p =
  let open Term in
  ret (const Lwt_main.run $ p)

(* Check if at least 2/3 of the cluster is in sync *)
let are_nodes_in_sync node_uris =
  let%await response =
    node_uris |> Lwt_list.map_p (fun node_uri -> request_in_sync () node_uri)
  in
  let number_of_nodes_in_sync =
    response
    |> List.fold_left
         (fun acc res ->
           let open Network.In_sync in
           match res with
           | { in_sync = true; _ } -> acc + 1
           | { in_sync = false; _ } -> acc)
         0
    |> float_of_int in
  number_of_nodes_in_sync
  >= float_of_int (List.length node_uris) *. (2.0 /. 3.0)
  |> await

let broadcast_bootstrap_signal node_uris producer =
  node_uris
  |> Lwt_list.iter_p (fun node_uri ->
         request_bootstrap_signal { producer } node_uri)

let bootstrap node_uris producer =
  print_endline (Crypto.Key_hash.to_string producer);
  let%await in_sync = are_nodes_in_sync node_uris in
  match in_sync with
  | true -> Lwt.return (`Ok ())
  | false ->
    let%await () = broadcast_bootstrap_signal node_uris { address = producer } in
    Lwt.return (`Ok ())

let bootstrap =
  let open Term in
  let producer =
    let parser string =
      Crypto.Key_hash.of_string string
      |> Option.to_result
           ~none:(`Msg "can't parse the key hash of the producer") in
    let printer fmt address =
      Format.fprintf fmt "%s" (Crypto.Key_hash.to_string address) in
    let open Arg in
    let producer = conv (parser, printer) in
    let docv = "producer" in
    let doc =
      "The address of a validator which will produce a block to bootstrap the \
       cluster." in
    required & pos 0 (some producer) None & info [] ~docv ~doc in
  (* TODO: Replace this list by an argument *)
  let node_uris =
    const
      [
        Uri.of_string "http://localhost:4440";
        Uri.of_string "http://localhost:4441";
        Uri.of_string "http://localhost:4442";
      ] in
  lwt_ret (const bootstrap $ node_uris $ producer)

let info =
  let doc = "Deku bootstrapper" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "side-cli" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ = Cmd.eval @@ Cmd.v info bootstrap
