open Helpers

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
    (let%await () = Lwt_unix.sleep 5.0 in
     Flows.handle_consensus_operation Consensus.with_timeout;
     reset_timeout server;
     Lwt.return_unit)

let task_pool = ref None

let get_task_pool () =
  match !task_pool with
  | Some pool -> pool
  | None ->
    (* TODO: proper number for additional domains *)
    let pool = Domainslib.Task.setup_pool ~num_additional_domains:8 () in
    let () = task_pool := Some pool in
    pool

let () = Flows.reset_timeout := fun () -> reset_timeout (get ())

let () = Flows.get_state := get_state

let () = Flows.set_state := set_state

let () = Flows.get_task_pool := get_task_pool
