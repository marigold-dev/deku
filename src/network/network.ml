open Broadcast

type network =
  | Network of { nodes : Uri.t list; known_messages : Message_hash.Set.t }

type t = network

let make ~nodes = Network { nodes; known_messages = Message_hash.Set.empty }

exception Duplicated_message
exception Invalid_hash

let incoming_message (type a) ~(endpoint : a Endpoint.t) ~message network :
    a option * network =
  let message_json = Yojson.Safe.from_string message in
  let (Message { hash; content } as message) =
    Message.t_of_yojson message_json
  in

  let (Network { nodes; known_messages }) = network in
  match Message_hash.Set.mem hash known_messages with
  | true -> raise Duplicated_message
  | false -> (
      match Message.verify message with
      | true -> (
          let known_messages = Message_hash.Set.add hash known_messages in
          let network = Network { nodes; known_messages } in

          match
            (* TODO: really important, how to prevent spam?
                Can the same parsed data be derived from two different hashes? *)
            (* TODO: does it make sense here? *)
            let () = broadcast_json ~nodes ~endpoint ~message:message_json in
            Message.content_of_yojson ~endpoint content
          with
          | content -> (Some content, network)
          | exception _exn -> (* TODO: dump exception*) (None, network))
      | false -> (* TODO: spam prevention *) raise Invalid_hash)

let incoming_message ~endpoint ~message network =
  match incoming_message ~endpoint ~message network with
  | message, network -> (message, network)
  | exception _exn -> (* TODO: dump exception*) (None, network)

let broadcast ~endpoint ~content network =
  let (Network { nodes; known_messages }) = network in
  let message =
    let content = Message.yojson_of_content ~endpoint content in
    Message.make ~content
  in
  let () = broadcast_message ~nodes ~endpoint ~message in
  Network { nodes; known_messages }

let broadcast_block ~block network =
  broadcast ~endpoint:Endpoint.blocks ~content:block network

let broadcast_signature ~signature network =
  broadcast ~endpoint:Endpoint.signatures ~content:signature network

let broadcast_operation ~operation network =
  broadcast ~endpoint:Endpoint.operations ~content:operation network
