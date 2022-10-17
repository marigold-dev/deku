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
  let repr = Signed_operation.of_signed operation in
  let json = Signed_operation.yojson_of_t repr |> Yojson.Safe.to_string in
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env api_url in
  match post_result with
  | Ok _ -> Ok ()
  | Error err -> Error (Piaf.Error.to_string err)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Operation in
  Parallel.Pool.run ~env ~domains:8 @@ fun () ->
  let api_url = "http://localhost:8080/api/v1/operations" |> Uri.of_string in
  let identity =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in
  let nonce =
    let rng = Stdlib.Random.State.make_self_init () in
    Stdlib.Random.State.bits64 rng
    |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n
  in
  let level = Level.zero in
  let operation = Signed.noop ~identity ~level ~nonce in
  let res = post_to_api ~sw ~env ~api_url operation in
  match res with
  | Error err ->
      print_endline err;
      exit 1
  | Ok _ ->
      print_endline "success";
      exit 0
