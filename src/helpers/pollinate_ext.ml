(** Temporary module
    DO NOT COMMIT AND PUSH TO MAIN *)

let address_to_string addr =
  let open Pollinate.Address in
  Printf.sprintf "%s:%d" addr.address addr.port

let to_string pnode =
  let open Pollinate.PNode in
  Printf.sprintf "Node (%s)" (address_to_string (Client.address_of pnode))

let peer_to_string peer =
  let open Pollinate.Peer in
  Printf.sprintf "Peer (%s)" (address_to_string (peer.address))

let iter_peers pnode f =
  let open Pollinate.PNode in
  Base.Hashtbl.iter ~f (Client.peers_of pnode)
