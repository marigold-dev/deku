open Deku_consensus
open Api_state
open Deku_indexer

module type HANDLER = sig
  type input
  (** The input of your handler: body, params, etc...*)

  type response [@@deriving yojson_of]
  (** The response of your handler *)

  val path : string
  (** The path of your endpoint *)

  val meth : [> `POST | `GET ]
  (** The method of your endpoint *)

  val input_from_request : Dream.request -> (input, string) result Lwt.t
  (** Parsing function of the request to make an input *)

  val handle : input -> Api_state.t -> response Lwt.t
  (** handler logic *)
end

(** Listen to the deku-node for new blocks *)
module Listen_blocks : HANDLER = struct
  type input = Block.t [@@deriving of_yojson]
  type response = unit [@@deriving yojson_of]

  let path = "/listen/blocks"
  let meth = `POST

  let input_from_request request =
    Api_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle block state =
    let { indexer; _ } = state in
    Indexer.save_block ~block indexer;
    Lwt.return_unit
end
