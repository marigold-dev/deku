open Deku_consensus
open Deku_indexer
open Deku_network
open Deku_concepts
open Deku_protocol

type api_state = {
  consensus_address : Deku_tezos.Address.t;
  mutable current_block : Block.t;
  indexer : Indexer.t;
  network : Network_manager.t;
  identity : Identity.t;
  protocol : Protocol.t;
}

type t = api_state

let make ~consensus_address ~indexer ~network ~identity ~protocol =
  {
    consensus_address;
    current_block = Genesis.block;
    indexer;
    network;
    identity;
    protocol;
  }
