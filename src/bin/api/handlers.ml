open Deku_concepts
open Deku_stdlib

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
  type input = Level.t [@@deriving of_yojson]
  type response = unit [@@deriving yojson_of]

  let path = "/listen/blocks"
  let meth = `POST

  let input_from_request request =
    Api_utils.input_of_body ~of_yojson:input_of_yojson request

  let handle level _ =
    print_endline
      (Format.sprintf "A new block has been applied on level %s"
         (level |> Level.to_n |> N.show));
    Lwt.return_unit
end