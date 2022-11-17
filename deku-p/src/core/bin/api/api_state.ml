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
  dump :
    current_block:Block.t ->
    protocol:Protocol.t ->
    receipts:Receipt.t Operation_hash.Map.t ->
    unit;
}

type t = api_state

let make ~consensus_address ~indexer ~network ~identity ~protocol ~current_block
    ~receipts ~dump =
  {
    consensus_address;
    current_block;
    indexer;
    network;
    identity;
    protocol;
    is_sync = false;
    receipts;
    dump;
  }

let find_withdraw_proof ~operation_hash state =
  let receipts = state.receipts in
  let (Protocol { ledger; _ }) = state.protocol in
  match Operation_hash.Map.find_opt operation_hash receipts with
  | None -> Error `Unknown_operation
  | Some (Withdraw_receipt { operation = _; handle }) ->
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

  let encoding =
    let open Data_encoding in
    conv
      (fun { current_block; protocol; receipts } ->
        (current_block, protocol, receipts))
      (fun (current_block, protocol, receipts) ->
        { current_block; protocol; receipts })
      (tup3 Block.encoding Protocol.encoding
         (Operation_hash.Map.encoding Receipt.encoding))

  let read ~env ~folder =
    let file = Filename.concat folder file in
    match IO.file_exists file with
    | false -> None
    | true ->
        let fs = Eio.Stdenv.fs env in
        let file = Eio.Path.(fs / file) in
        Eio.Path.with_open_in file @@ fun source ->
        let buf = Cstruct.create 4096 in
        let rec decoding_loop status =
          let open Data_encoding.Binary in
          match status with
          | Success { result; _ } -> result
          | Await feed ->
              let size = Eio.Flow.read source buf in
              let status = feed (Cstruct.to_bytes ~len:size buf) in
              decoding_loop status
          | Error err -> raise (Read_error err)
        in
        let chain = decoding_loop (Data_encoding.Binary.read_stream encoding) in
        Some chain

  let write ~env ~data_folder ~current_block ~protocol ~receipts =
    let state = { current_block; protocol; receipts } in
    let fs = Eio.Stdenv.fs env in
    let file = Eio.Path.(fs / data_folder / file) in
    let temp = Eio.Path.(fs / data_folder / temp) in
    let binary = Data_encoding.Binary.to_string_exn encoding state in
    Eio.Path.save ~create:(`Or_truncate 0o6444) temp binary;
    Eio.Path.rename temp file
end
