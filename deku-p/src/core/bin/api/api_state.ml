open Deku_consensus
open Deku_indexer
open Deku_network
open Deku_concepts
open Deku_protocol
open Deku_stdlib

type api_state = {
  consensus_address : Deku_tezos.Address.t;
  mutable current_block : Block.t;
  indexer : Indexer.t;
  network : Network_manager.t;
  identity : Identity.t;
  mutable protocol : Protocol.t;
}

type t = api_state

let make ~consensus_address ~indexer ~network ~identity ~protocol ~current_block
    =
  { consensus_address; current_block; indexer; identity; network; protocol }

module Storage = struct
  type t = { current_block : Block.t; protocol : Protocol.t }
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
    } =
      state
    in
    { current_block; protocol }

  let read ~env:_ ~folder =
    let file = Filename.concat folder file in
    match IO.file_exists file with
    | true ->
        let state =
          Eio_unix.run_in_systhread (fun () ->
              let json = Yojson.Safe.from_file file in
              t_of_yojson json)
        in
        Some state
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
