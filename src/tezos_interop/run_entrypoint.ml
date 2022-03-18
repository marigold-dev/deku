open Helpers
open Crypto
open Tezos

type input = {
  rpc_node : string;
  secret : string;
  confirmation : int;
  destination : string;
  entrypoint : string;
  payload : Yojson.Safe.t;
}
[@@deriving to_yojson]
type output =
  | Applied     of { hash : string }
  | Failed      of { hash : string }
  | Skipped     of { hash : string }
  | Backtracked of { hash : string }
  | Unknown     of { hash : string }
  | Error       of string
let output_of_yojson json =
  let module T = struct
    type t = { status : string } [@@deriving of_yojson { strict = false }]

    and finished = { hash : string }

    and error = { error : string }
  end in
  let finished make =
    let%ok { hash } = T.finished_of_yojson json in
    Ok (make hash) in
  let%ok { status } = T.of_yojson json in
  match status with
  | "applied" -> finished (fun hash -> Applied { hash })
  | "failed" -> finished (fun hash -> Failed { hash })
  | "skipped" -> finished (fun hash -> Skipped { hash })
  | "backtracked" -> finished (fun hash -> Backtracked { hash })
  | "unknown" -> finished (fun hash -> Unknown { hash })
  | "error" ->
    let%ok { error } = T.error_of_yojson json in
    Ok (Error error)
  | _ -> Error "invalid status"
let file = Scripts.file_run_entrypoint
let run ~rpc_node ~secret ~required_confirmations ~destination ~entrypoint
    ~payload =
  let input =
    {
      rpc_node = Uri.to_string rpc_node;
      secret = Secret.to_string secret;
      confirmation = required_confirmations;
      destination = Address.to_string destination;
      entrypoint;
      payload;
    } in
  let command = "node" in
  let%await output =
    Lwt_process.pmap
      (command, [|command; file|])
      (Yojson.Safe.to_string (input_to_yojson input)) in
  match Yojson.Safe.from_string output |> output_of_yojson with
  | Ok data ->
    Format.eprintf "Commit operation result: %s\n%!" output;
    await data
  | Error error -> await (Error error)
