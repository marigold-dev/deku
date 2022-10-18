open Deku_concepts
open Deku_crypto
open Deku_protocol
open Deku_stdlib

(* FIXME: this code is duplicated *)
module Signed_operation = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }
  [@@deriving yojson]

  let of_signed signed =
    let (Operation.Signed.Signed_operation { key; signature; initial }) =
      signed
    in
    { key; signature; initial }

  let to_signed repr =
    let { key; signature; initial } = repr in
    Operation.Signed.make_with_signature ~key ~signature ~initial
end

let post_to_api ~sw ~env ~api_url operation =
  let submit_op_uri = Uri.with_path api_url "/api/v1/operations" in
  let repr = Signed_operation.of_signed operation in
  let json = Signed_operation.yojson_of_t repr |> Yojson.Safe.to_string in
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env submit_op_uri in
  match post_result with
  | Ok Piaf.Response.{ status; _ } -> (
      match Piaf.Status.is_successful status with
      | true -> Ok ()
      | false ->
          Error
            (Format.sprintf "receive code response: %s"
               (Piaf.Status.to_string status)))
  | Error err -> Error (Piaf.Error.to_string err)

type level_response = { level : Level.t } [@@deriving of_yojson]

let level ~sw ~env ~api_url =
  let level_uri = Uri.with_path api_url "/api/v1/chain/level" in
  let result = Piaf.Client.Oneshot.get ~sw env level_uri in
  match result with
  | Ok Piaf.Response.{ status; body; _ } -> (
      match Piaf.Status.is_successful status with
      | true -> (
          let result = Piaf.Body.to_string body in
          match result with
          | Ok string ->
              let json = Yojson.Safe.from_string string in
              let { level } = level_response_of_yojson json in
              Ok level
          | Error err -> Error (Piaf.Error.to_string err))
      | false ->
          Error
            (Format.sprintf "receive code response: %s"
               (Piaf.Status.to_string status)))
  | Error err -> Error (Piaf.Error.to_string err)

type params = {
  domains : int; [@default 8]
  api_url : Uri.t; [@default Uri.of_string "http://localhost:8080"]
  secret : Ed25519.Secret.t; [@default Ed25519.Secret.generate ()]
}
[@@deriving cmdliner]

let main params =
  let { domains; api_url; secret } = params in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Operation in
  Parallel.Pool.run ~env ~domains @@ fun () ->
  let identity = Identity.make (Secret.Ed25519 secret) in
  let nonce =
    let rng = Stdlib.Random.State.make_self_init () in
    Stdlib.Random.State.bits64 rng
    |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n
  in
  let level = level ~sw ~env ~api_url in
  let level =
    match level with
    | Ok level -> level
    | Error str ->
        print_endline str;
        exit 1
  in
  let operation = Signed.noop ~identity ~level ~nonce in
  let res = post_to_api ~sw ~env ~api_url operation in
  let (Operation.Operation { hash; _ }) = operation in
  print_endline (Operation_hash.to_b58 hash);

  match res with
  | Error err ->
      print_endline err;
      exit 1
  | Ok _ ->
      print_endline "success";
      exit 0

let () =
  let info = Cmdliner.Cmd.info "deku-load-tester" in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
