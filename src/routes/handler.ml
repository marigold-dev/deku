open Deku_network
open Deku_chain
open Deku_stdlib
open Deku_indexer

(* Endpoint handlers should implement this signature
   and be generated with the Make_handler
*)
module type HANDLER = sig
  type input
  type output [@@deriving yojson_of]

  val handler : chain:Chain.t -> input -> (output, Internal_error.t) result
end

module Make_handler (Handler : HANDLER) : sig
  val handle : path:string -> chain:Chain.t -> Handler.input -> Piaf.Response.t
end = struct
  let handle ~path ~chain input =
    Log.info "handle request in %s" path;
    let result = Handler.handler ~chain input in
    match result with
    | Error err ->
        Log.error "error in path %s: %s" path (Internal_error.to_string err);
        Internal_error.to_response err
    | Ok output ->
        let body = Handler.yojson_of_output output |> Yojson.Safe.to_string in
        Piaf.Response.of_string ~body `OK
end

module type HANDLER_WITH_INDEXER = sig
  type input
  type output [@@deriving yojson_of]

  val handler :
    chain:Chain.t ->
    indexer:Indexer.t ->
    input ->
    (output, Internal_error.t) result Lwt.t
end

module Make_handler_with_indexer (Handler : HANDLER_WITH_INDEXER) : sig
  val handle :
    path:string ->
    chain:Chain.t ->
    indexer:Indexer.t ->
    Handler.input ->
    Piaf.Response.t Lwt.t
end = struct
  let handle ~path ~chain ~indexer input =
    Log.info "Handle request in %s" path;
    let%await result = Handler.handler ~chain ~indexer input in
    let response =
      match result with
      | Error err ->
          Log.error "error in path %s: %s" path (Internal_error.to_string err);
          Internal_error.to_response err
      | Ok output ->
          let body = Handler.yojson_of_output output |> Yojson.Safe.to_string in
          Piaf.Response.of_string ~body `OK
    in
    Lwt.return response
end

module Get_block_by_level = Make_handler_with_indexer (struct
  open Deku_concepts
  open Deku_consensus

  type input = Level.t
  type output = Block.t [@@deriving yojson_of]

  let handler ~chain ~indexer block_level =
    let _ = chain in
    let%await block = Indexer.find_block_by_level ~block_level indexer in
    let output =
      match block with
      | Some block -> Ok block
      | None -> Error Internal_error.block_not_found
    in
    Lwt.return output
end)

module Get_genesis = Make_handler (struct
  open Deku_consensus

  type input = unit
  type output = Block.t [@@deriving yojson_of]

  let handler ~chain () =
    let _ = chain in
    Ok Genesis.block
end)

module Get_chain_level = Make_handler (struct
  open Deku_concepts
  open Deku_chain

  type input = unit
  type output = { level : Level.t } [@@deriving yojson_of]

  let handler ~chain () =
    let (Chain.Chain { consensus; _ }) = chain in
    let (Consensus { current_level; _ }) = consensus in
    Ok { level = current_level }
end)