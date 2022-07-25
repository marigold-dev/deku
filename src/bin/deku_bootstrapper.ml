open Cmdliner
open Helpers
open Network
open Crypto

(* TODO: creates bin/common.ml, this code appears in three different places, deku_cli, deku_node, here *)
let lwt_ret p =
  let open Term in
  ret (const Lwt_main.run $ p)

let secret =
  let parser string =
    Secret.of_string string
    |> Option.to_result ~none:(`Msg "Cannot parse the bootstrapper secret")
  in
  let printer fmt secret = Format.fprintf fmt "%s" (Secret.to_string secret) in
  let open Arg in
  let secret = conv (parser, printer) in
  let docv = "secret" in
  let doc = "Secret key of the bootstrapper" in
  required & opt (some secret) None & info ["secret"] ~docv ~doc

module Bootstrap = struct
  (* TODO: should this be 2/3rd's plus 1? *)
  (* Check if at least 2/3 of the cluster is in sync *)
  let are_nodes_in_sync node_uris =
    let%await response =
      node_uris |> Lwt_list.map_p (fun node_uri -> request_in_sync () node_uri)
    in
    let number_of_nodes_in_sync =
      response
      |> List.filter (fun In_sync.{ in_sync } -> in_sync)
      |> List.length
      |> float_of_int in
    number_of_nodes_in_sync
    >= float_of_int (List.length node_uris) *. (2.0 /. 3.0)
    |> await

  let broadcast_bootstrap_signal node_uris msg =
    node_uris
    |> Lwt_list.iter_p (fun node_uri -> request_bootstrap_signal msg node_uri)

  let bootstrap node_uris producer secret =
    let%await in_sync = are_nodes_in_sync node_uris in
    match in_sync with
    | true ->
      (* TODO: use Logs everywhere. *)
      Format.printf "Nodes are in sync. Doing nothing.\n%!";
      Lwt.return (`Ok ())
    | false ->
      let signature =
        BLAKE2B.hash (Key_hash.to_string producer)
        |> Protocol.Signature.sign ~key:secret in
      let%await () =
        broadcast_bootstrap_signal node_uris
          { producer = { address = producer }; signature } in
      Lwt.return (`Ok ())

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
    required & pos 0 (some producer) None & info [] ~docv ~doc

  let node_uris =
    let parser string =
      String.split_on_char ';' string |> List.map Uri.of_string |> Result.ok
    in
    let printer fmt node_uris =
      Format.fprintf fmt "%s"
        (String.concat ";" (node_uris |> List.map Uri.to_string)) in
    let open Arg in
    let node_uris = conv (parser, printer) in
    let docv = "identity" in
    let doc = "List of uri separated by ';'" in
    let default =
      Some
        [
          Uri.of_string "http://localhost:4440";
          Uri.of_string "http://localhost:4441";
          Uri.of_string "http://localhost:4442";
        ] in
    required & opt (some node_uris) default & info ["node-uris"] ~docv ~doc

  let run =
    let open Term in
    lwt_ret (const bootstrap $ node_uris $ producer $ secret)

  let info =
    let doc = "Create the bootstrapper identity" in
    Cmd.info "bootstrap" ~version:"%\226\128\140%VERSION%%" ~doc
end

module Setup_identity = struct
  let setup_identity () =
    let secret, key = Crypto.Ed25519.generate () in
    let secret, key = (Secret.Ed25519 secret, Key.Ed25519 key) in
    (* TODO: use Logs everywhere *)
    Format.printf "Key: %s\nSecret: %s\n%!" (Key.to_string key)
      (Secret.to_string secret);
    await (`Ok ())

  let run =
    let open Term in
    lwt_ret (const setup_identity $ const ())

  let info =
    let doc = "Create the bootstrapper identity" in
    Cmd.info "setup-identity" ~version:"%\226\128\140%VERSION%%" ~doc
end

module Derive_secret = struct
  let derive_secret secret =
    let key = Key.of_secret secret in
    (* TODO: use Logs everywhere *)
    Format.printf "Key: %s\n" (Key.to_string key);
    await (`Ok ())

  let run =
    let open Term in
    lwt_ret (const derive_secret $ secret)

  let info =
    let doc = "Derive the public key from the given secret" in
    Cmd.info "derive-secret" ~version:"%\226\128\140%VERSION%%" ~doc
end

let default_info =
  let doc = "Deku bootstrapper" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "side-cli" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  Cmd.eval
  @@ Cmd.group default_info
       [
         Cmd.v Bootstrap.info Bootstrap.run;
         Cmd.v Setup_identity.info Setup_identity.run;
         Cmd.v Derive_secret.info Derive_secret.run;
       ]
