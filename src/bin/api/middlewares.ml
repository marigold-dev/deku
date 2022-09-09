open Deku_stdlib

(* Add the correct headers for cors policy *)
let cors handler req =
  let%await response = handler req in
  Dream.add_header response "Access-Control-Allow-Origin" "*";
  Dream.add_header response "Access-Control-Allow-Headers" "*";
  Dream.add_header response "Allow" "*";
  Lwt.return response
