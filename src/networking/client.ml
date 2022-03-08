let create_client port =
  let open Lwt_unix in
  let client_socket = socket PF_INET SOCK_DGRAM 0 in
  let client_addr = ADDR_INET (Unix.inet_addr_loopback, port) in
  let%lwt () = bind client_socket client_addr in
  Lwt.return client_socket

let send_message = failwith "undefined"
