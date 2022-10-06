open Deku_consensus
open Deku_indexer
open Deku_network

type api_state = {
  consensus_address : Deku_tezos.Address.t;
  node_address : string;
  node_port : int;
  mutable current_block : Block.t;
  indexer : Indexer.t;
  network : Network_manager.t;
}

type t = api_state

let make ~consensus_address ~indexer ~node_port ~network =
  {
    consensus_address;
    node_address = "127.0.0.1";
    node_port;
    current_block = Genesis.block;
    indexer;
    network;
  }