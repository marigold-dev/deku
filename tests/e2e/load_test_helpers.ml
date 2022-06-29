open Crypto
open Node
open Helpers
open Cmdliner

open (
  struct
    include Server
  end :
    sig end)

type wallet = {
  key_hash : Key_hash.t;
  secret : Secret.t;
}

(* The wallets are hard-coded to make it easier to deposit the initial ticket
   and to enable expect tests on the output in the future. *)
let alice_wallet =
  {
    key_hash =
      Key_hash.of_string "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Option.get;
    secret =
      Secret.of_string "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
      |> Option.get;
  }

let bob_wallet =
  {
    key_hash =
      Key_hash.of_string "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob" |> Option.get;
    secret =
      Secret.of_string "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"
      |> Option.get;
  }

(* Hard-coded for now. TODO: get these dynamically, see
   https://github.com/marigold-dev/deku/pull/450 *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int

let get_random_validator_uri () =
  let validator =
    List.nth validators_uris (random_int (List.length validators_uris)) in
  validator |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things quite
   a bit, since we don't have to query the contents of the ticket or serialize
   and then parse the bytes *)
let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core_deku.Ticket_id.{ ticketer; data = Bytes.empty }

let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  let amount = Core_deku.Amount.of_int amount in
  Protocol.Operation.Core_user.sign ~secret:sender.secret
    ~nonce:(Crypto.Random.int32 Int32.max_int)
    ~block_height:block_level
    ~data:
      (Core_deku.User_operation.make ~source:sender.key_hash
         (Transaction { destination = recipient.key_hash; amount; ticket }))

module Test_kind = struct
  (* TODO: this is a lot of boiler plate :( PPX to help with this? *)
  type t =
    | Saturate
    | Maximal_blocks

  let all_options = ["saturate"; "maximal-blocks"]

  let of_string = function
    | "saturate" -> Ok Saturate
    | "maximal-blocks" -> Ok Maximal_blocks
    | s -> Error (Format.sprintf "Unable to parse test kind \"%s\"" s)

  let to_string = function
    | Saturate -> "saturate"
    | Maximal_blocks -> "maximal-blocks"

  let test_kind_conv =
    let parser x = of_string x |> Result.map_error (fun e -> `Msg e) in
    let printer ppf test_kind = Format.fprintf ppf "%s" (to_string test_kind) in
    let open Arg in
    conv (parser, printer)

  let arg_info =
    let docv = "test_kind" in
    let doc =
      "The type of test to perform. Options: " ^ String.concat " | " all_options
    in
    Arg.info [] ~doc ~docv
end