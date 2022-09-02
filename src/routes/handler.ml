open Deku_chain
open Deku_network
open Deku_stdlib

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

module Get_block_by_level = Make_handler (struct
  open Deku_concepts
  open Deku_consensus

  type input = Level.t
  type output = Block.t [@@deriving yojson_of]

  let handler ~chain level =
    (* TODO: get the block from the chain at the current level *)
    let _ = level in
    let _ = chain in
    Genesis.block
    |> Result.ok (* Should be replaced by the block at the given level *)
end)
