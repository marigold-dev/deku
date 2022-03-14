open Helpers
open Flows
type t = {
  mutable state : State.t;
  mutable timeout : unit Lwt.t;
}
let global_server = ref None
let start ~initial =
  match !global_server with
  | Some _ -> failwith "start should be called just once"
  | None -> global_server := Some { state = initial; timeout = Lwt.return_unit }
let get () =
  match !global_server with
  | Some state -> state
  | None -> failwith "get called before start"
let get_port () = (get ()).state.identity.uri |> Uri.port
let get_state () = (get ()).state
let set_state state = (get ()).state <- state
let rec reset_timeout server =
  Lwt.cancel server.timeout;
  server.timeout <-
    (let%await () = Lwt_unix.sleep 10.0 in
     (match
        try_to_produce_block server.state (fun state ->
            server.state <- state;
            state)
      with
     | Ok () -> ()
     | Error `Not_current_block_producer -> ());
     reset_timeout server;
     Lwt.return_unit)
let () = Flows.reset_timeout := fun () -> reset_timeout (get ())
let () = Flows.get_state := get_state
let () = Flows.set_state := set_state
