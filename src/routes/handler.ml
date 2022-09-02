open Deku_chain
open Deku_network

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
    let _ = path in
    let result = Handler.handler ~chain input in
    match result with
    | Error err -> Internal_error.to_response err
    | Ok output ->
        let body = Handler.yojson_of_output output |> Yojson.Safe.to_string in
        Piaf.Response.of_string ~body `OK
end
