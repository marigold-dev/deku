open Lwt_unix

type client_state =
  | Ok
  | Suspicious
  | Failed

type client = {
  state : client_state;
  known_clients : client list;
  address : Lwt_unix.sockaddr;
  port : int;
}

(* Basic Knuth shuffle => https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)
let knuth_shuffle_client_list client_list =
  let initial_array = Array.length @@ Array.of_list client_list in
  let copied_array = Array.copy @@ Array.of_list client_list in
  for i = initial_array - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = copied_array.(k) in
    copied_array.(k) <- copied_array.(i);
    copied_array.(i) <- x
  done;
  Array.to_list copied_array

let add_client client_to_add client =
  let new_client_list : client list = client_to_add :: client.known_clients in
  let shuffled_list = knuth_shuffle_client_list new_client_list in
  {
    state = client.state;
    known_clients = shuffled_list;
    address = client.address;
    port = client.port;
  }
