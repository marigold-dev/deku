open Cmdliner
open Crypto
open Node
open Helpers

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

let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let get_random_validator_uri () =
  List.nth validators_uris (Stdlib.Random.int 3) |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let block_level =
    Lwt_main.run @@ Networking.request_block_level () validator_uri in
  block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things
   quite a bit, since we don't have to query the contents of the ticket
   or serialize and then parse the bytes *)
let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core.Ticket_id.{ ticketer; data = Bytes.empty }

let nonce = ref 0l

let do_transaction ~validator_uri ~block_level ~sender =
  nonce := Int32.add 1l !nonce;
  let payload = {|{"Action":"Increment"}|} in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:sender.secret ~nonce:!nonce
      ~block_height:block_level
      ~data:
        (Core.User_operation.make
           ~sender:(Core.Address.of_key_hash sender.key_hash)
           (Vm_transaction { payload = Yojson.Safe.from_string payload })) in
  Lwt.async (fun () ->
      Networking.request_user_operation_gossip
        { user_operation = transaction }
        validator_uri);
  transaction

let spam_transactions n () =
  (* Hard-coded for now. TODO: get these dynamically, see https://github.com/marigold-dev/deku/pull/450 *)
  let validators_uris =
    ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]
  in
  let get_random_validator_uri () =
    List.nth validators_uris (Stdlib.Random.int 3) |> Uri.of_string in
  List.init n (fun _ ->
      let validator_uri = get_random_validator_uri () in
      let block_level = get_current_block_level () in
      do_transaction ~validator_uri ~block_level ~sender:alice_wallet)

module Test_kind = struct
  (* TODO: this is a lot of boiler plate :(
     PPX to help with this? *)
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

let load_test_transactions test_kind =
  let test =
    match test_kind with
    | Test_kind.Saturate ->
      let n = 10000 in
      Format.printf "Running %i ticket transfers\n%!" n;
      spam_transactions n
    | Test_kind.Maximal_blocks ->
      (* TODO: write a test that maximally packs blocks and transmits them. *)
      assert false in
  let starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  let _ = test () in
  await ()

let load_test_transactions test_kind =
  load_test_transactions test_kind |> Lwt_main.run

let args =
  let open Arg in
  let test_kind =
    required & pos 0 (some Test_kind.test_kind_conv) None & Test_kind.arg_info
  in
  let open Term in
  const load_test_transactions $ test_kind

let () = Term.exit @@ Term.eval (args, Term.info "load-test")
