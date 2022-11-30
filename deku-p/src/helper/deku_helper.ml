open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Deku_crypto

let post_directly_to_node ~env ~operation =
  let host = "127.0.0.1" in
  let port = 4440 in
  let net = Eio.Stdenv.net env in
  let content = Message.Content.operation operation in
  let (Message { network; _ }) = Message.encode ~content in
  let (Network_message { raw_header; raw_content }) = network in
  let open Deku_network in
  let message = Network_message.message ~raw_header ~raw_content in
  Network_protocol.Client.connect ~net ~host ~port @@ fun connection ->
  Network_protocol.Connection.write connection message

let post_to_api ~sw ~env ~operation =
  let repr = Repr.Signed_operation.of_signed operation in
  let json =
    Repr.Signed_operation.yojson_of_t repr |> Yojson.Safe.pretty_to_string
  in
  Format.eprintf "%s\n%!" json;
  let node = "http://localhost:8080/api/v1/operations" |> Uri.of_string in
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env node in
  match post_result with
  | Ok _ -> print_endline "operation submitted"
  | Error _ -> print_endline "FAIL to submit operation"

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

type level_response = { level : Level.t } [@@deriving of_yojson]

let make_level ~sw ~env () =
  let response =
    Piaf.Client.Oneshot.get ~sw env
      (Uri.of_string "http://localhost:8080/api/v1/chain/level")
  in
  let body =
    match response with
    | Error _ -> failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let string = Piaf.Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let yojson = Yojson.Safe.from_string body in
  let { level } = level_response_of_yojson yojson in
  level

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

type params = { input : Deku_gameboy.Joypad.t } [@@deriving cmdliner]

let main params =
  let { input } = params in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let identity =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in
  let level = make_level ~sw ~env () in
  let nonce = Nonce.of_n N.zero in
  let op = Operation.Signed.joypad_input ~level ~nonce ~input ~identity in
  let _ = post_to_api ~sw ~env ~operation:op in
  exit 0

let () =
  let open Cmdliner in
  let info = Cmd.info "deku-helper" in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval ~catch:true cmd)
