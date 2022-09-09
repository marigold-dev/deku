open Deku_stdlib

(* Add the correct headers for cors policy *)
let cors handler req =
  let%await response = handler req in
  Dream.add_header response "Access-Control-Allow-Origin" "*";
  Dream.add_header response "Access-Control-Allow-Headers" "*";
  Dream.add_header response "Allow" "*";
  Lwt.return response

(* Adds header to prevent cache on some get routes *)
let no_cache handler req =
  let%await response = handler req in
  print_endline "add Cache-Control";
  Dream.add_header response "Cache-Control" "max-age=0, no-cache, no-store";
  Lwt.return response
