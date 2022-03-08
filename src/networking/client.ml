open Lwt_unix

let create_client port =
  let client_socket = Lwt_unix.socket PF_INET SOCK_DGRAM 0 in
  let client_addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let%lwt () = Lwt_unix.bind client_socket client_addr in
  Lwt.return client_socket

let send_message = failwith "undefined"
