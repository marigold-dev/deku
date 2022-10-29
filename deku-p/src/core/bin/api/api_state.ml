open Deku_consensus
open Deku_block_storage
open Deku_network
open Deku_concepts
open Deku_protocol
open Deku_stdlib

type api_state = {
  consensus_address : Deku_tezos.Address.t;
  mutable current_block : Block.t;
  indexer : Block_storage.t;
  network : Network_manager.t;
  identity : Identity.t;
  mutable protocol : Protocol.t;
  mutable is_sync : bool;
  mutable receipts : Receipt.t Operation_hash.Map.t;
      (** Receipts of the included operations; also contains withdrawal receipts, which are used to
          generate withdrawal proofs. *)
}

type t = api_state

let make ~consensus_address ~indexer ~network ~identity ~protocol ~current_block
    ~receipts =
  {
    consensus_address;
    current_block;
    indexer;
    network;
    identity;
    protocol;
    is_sync = false;
    receipts;
  }

let find_withdraw_proof ~operation_hash state =
  let receipts = state.receipts in
  let (Protocol { ledger; _ }) = state.protocol in
  match Operation_hash.Map.find_opt operation_hash receipts with
  | None -> Error `Unknown_operation
  | Some
      (Withdraw_receipt
        { operation = _; account = _; ticket_id = _; amount = _; handle }) ->
      let open Deku_ledger in
      let withdrawal_handles_hash =
        Ledger.withdrawal_handles_root_hash ledger
      in
      Ok
        ( handle,
          Ledger.withdrawal_handles_find_proof handle ledger,
          withdrawal_handles_hash )
  | _ ->
      (* FIXME? fragile *)
      Logs.err (fun m -> m "Found a receipt that does not match");
      Error `Not_a_withdraw

module Storage = struct
  type t = {
    current_block : Block.t;
    protocol : Protocol.t;
    receipts : Receipt.t Operation_hash.Map.t;
  }
  [@@deriving yojson]

  let temp = "deku_api.tmp.json"
  let file = "deku_api.json"

  let state_to_storage state =
    let {
      consensus_address = _;
      current_block;
      indexer = _;
      identity = _;
      network = _;
      protocol;
      is_sync = _;
      receipts;
    } =
      state
    in
    { current_block; protocol; receipts }

  let read ~env:_ ~folder =
    let file = Filename.concat folder file in
    match IO.file_exists file with
    | true ->
        let json = Yojson.Safe.from_file file in
        Some (t_of_yojson json)
    | false -> None

  let write ~env ~folder state =
    Eio_unix.run_in_systhread (fun () ->
        let temp = Filename.concat folder temp in
        let state = state_to_storage state in
        let json = yojson_of_t state in
        Yojson.Safe.to_file temp json);
    let fs = Eio.Stdenv.fs env in
    let temp = Eio.Path.(fs / folder / temp) in
    let file = Eio.Path.(fs / folder / file) in
    Eio.Path.rename temp file
end
