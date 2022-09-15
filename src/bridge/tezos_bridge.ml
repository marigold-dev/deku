open Deku_stdlib
open Deku_crypto
open Deku_tezos

type bridge = {
  rpc_node : Uri.t;
  secret : Secret.t;
  consensus_contract : Address.t;
  js_bridge_process : Tezos_js_bridge.t;
}

type t = bridge

let make ~rpc_node ~secret ~consensus_contract =
  let js_bridge_process = Tezos_js_bridge.spawn () in
  { rpc_node; secret; consensus_contract; js_bridge_process }

module Run_contract = struct
  let run bridge ~destination ~entrypoint ~payload =
    let { rpc_node; secret; consensus_contract = _; js_bridge_process } =
      bridge
    in
    Tezos_js_bridge.inject_transaction js_bridge_process ~rpc_node ~secret
      ~destination ~entrypoint ~payload
end

module Consensus = struct
  let commit_state_hash bridge ~block_level ~block_content ~signatures =
    let module Payload = struct
      type t = {
        block_level : N.t;
        block_content : string;
        signatures : (string * string) option list;
      }
      [@@deriving yojson]
    end in
    let block_level = Deku_concepts.Level.to_n block_level in
    let block_content = BLAKE2b.to_hex block_content in
    let signatures =
      List.map
        (fun signature ->
          match signature with
          | Some (key, signature) ->
              let key = Key.to_b58 key in
              let signature = Signature.to_b58 signature in
              Some (key, signature)
          | None -> None)
        signatures
    in
    let payload = Payload.{ block_level; block_content; signatures } in
    (* TODO: check result *)
    let%await _ =
      Run_contract.run bridge ~destination:bridge.consensus_contract
        ~entrypoint:"update_root_hash"
        ~payload:(Payload.yojson_of_t payload)
    in
    Lwt.return_unit
end
