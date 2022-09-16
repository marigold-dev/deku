open Api_state
open Deku_concepts
open Deku_stdlib
open Deku_indexer
open Deku_consensus

module type HANDLER = sig
  type input
  (** The input of your handler: body, params, etc...*)

  type response [@@deriving yojson_of]
  (** The response of your handler *)

  val path : string
  (** The path of your endpoint *)

  val meth : [> `POST | `GET ]
  (** The method of your endpoint *)

  val input_from_request : Dream.request -> (input, Api_error.t) result Lwt.t
  (** Parsing function of the request to make an input *)

  val handle : input -> Api_state.t -> (response, Api_error.t) result Lwt.t
  (** handler logic *)
end

(** Listen to the deku-node for new blocks *)
module Listen_blocks : HANDLER = struct
  type input = Level.t [@@deriving of_yojson]
  type response = unit [@@deriving yojson_of]

  let path = "/listen/blocks"
  let meth = `POST

  let input_from_request request =
    Api_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle level state =
    let { indexer; _ } = state in
    let level = level |> Level.to_n |> N.to_z |> Z.to_int64 in
    let%await _block = Indexer.find_block ~level indexer in
    print_endline "Retrieve the new applied block from the chain";
    Lwt.return_ok ()
end

(* Return the nth block of the chain. *)
module Get_block : HANDLER = struct
  type input = int64 [@@deriving of_yojson]
  type response = Block.t [@@deriving yojson_of]

  let path = "/chain/blocks/:block"
  let meth = `GET

  let input_from_request request =
    Api_utils.param_of_request request "block"
    |> Option.to_result ~none:(Api_error.missing_parameter "block")
    |> Result.map Int64.of_string_opt
    |> Result.map
         (Option.to_result
            ~none:(Api_error.invalid_parameter "Level should be a string in64."))
    |> Result.join |> Lwt.return

  let handle request state =
    let { indexer } = state in
    let%await block = Indexer.find_block ~level:request indexer in
    match block with
    | Some block -> Lwt.return_ok block
    | None -> Lwt.return_error Api_error.block_not_found
end