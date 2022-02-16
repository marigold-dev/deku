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
let set_state state = (get ()).state <- state;;
Flows.get_state := get_state;;
Flows.set_state := set_state
