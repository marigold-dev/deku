type api_state = {
  consensus_address : Deku_tezos.Address.t;
  node_address : string;
  node_port : int;
}

type t = api_state

let make ~consensus_address ~node_port =
  { consensus_address; node_address = "127.0.0.1"; node_port }