open Crypto
open Protocol
open Consensus
module Address_map = Map.Make (Key_hash)
module Uri_map = Map.Make (Uri)

type pollinate_context =
  | Client
  | Server
type timestamp = float

type t = {
  config : Config.t;
  consensus : Consensus.t;
  interop_context : Tezos_interop.t;
  data_folder : string;
  (* TODO: we need a bound on the size of this and put
     behind an abstract type. We should also change how
     this works once we have an indexer. See https://github.com/marigold-dev/deku/issues/535 *)
  applied_blocks : (timestamp * Block.t) list;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
  persist_trusted_membership_change :
    Trusted_validators_membership_change.t list -> unit Lwt.t;
  pollinate_node : Pollinate.PNode.t ref Lwt.t;
}

let make ~config ~trusted_validator_membership_change
    ~persist_trusted_membership_change ~interop_context ~data_folder
    ~initial_validators_uri ~pollinate_context =
  let consensus =
  Consensus.make ~identity:config.Config.identity 
    ~trusted_validator_membership_change in
  let pollinate_node =
    match pollinate_context with
    | Client ->
      (* Client mode only *)
      Pollinate.PNode.init ~init_peers:[]
        (Pollinate.Address.create "127.0.0.1" 4000)
    | Server ->
      let uri_to_pollinate : Uri.t -> Pollinate.Address.t =
       fun uri ->
        let address =
          match Uri.host uri with
          | Some "localhost" -> "127.0.0.1"
          | Some address -> address
          | _ -> failwith "Could not retrieve address from uri" in
        let port =
          match Uri.port uri with
          | Some port -> port + 100 (* ugly fix to avoif using the HTTP port *)
          | None -> failwith "Could not retrieve port from uri." in
        Pollinate.Address.create address port in
      let pollinate_address = uri_to_pollinate config.Config.identity.uri in

      let init_peers =
        List.map
          (fun (_, x) -> uri_to_pollinate x)
          (Address_map.bindings initial_validators_uri) in

      let pollinate_node = Pollinate.PNode.init ~init_peers pollinate_address in
      pollinate_node in
  {
    config;
    consensus;
    interop_context;
    data_folder;
    applied_blocks = [];
    uri_state = Uri_map.empty;
    validators_uri = initial_validators_uri;
    recent_operation_receipts = BLAKE2B.Map.empty;
    persist_trusted_membership_change;
    pollinate_node;
  }
