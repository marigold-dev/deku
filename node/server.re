open Helpers;
open Flows;

// state related
// TODO: load snapshot
type t = {
  // TODO: lock state
  mutable state: State.t,
  mutable timeout: Lwt.t(unit),
};

let task_pool = ref(None);

let global_server = ref(None);
let start = (~initial) =>
  switch (global_server^) {
  | Some(_) => failwith("start should be called just once")
  | None => global_server := Some({state: initial, timeout: Lwt.return_unit})
  };

let get = () =>
  switch (global_server^) {
  | Some(state) => state
  | None => failwith("get called before start")
  };

let get_port = () => get().state.identity.uri |> Uri.port;

let get_state = () => get().state;
let set_state = state => get().state = state;
let rec reset_timeout = server => {
  Lwt.cancel(server.timeout);
  // TODO: this is dumb, should be frequent operation
  server.timeout = {
    let.await () = Lwt_unix.sleep(10.0);
    switch (
      try_to_produce_block(
        server.state,
        state => {
          server.state = state;
          state;
        },
      )
    ) {
    | Ok () => ()
    | Error(`Not_current_block_producer) => ()
    };
    reset_timeout(server);
    Lwt.return_unit;
  };
};

let setup_task_pool = () =>
  task_pool :=
    Some(Domainslib.Task.setup_pool(~num_additional_domains=4, ()));

let get_task_pool = () =>
  switch (task_pool^) {
  | Some(pool) => pool
  | None => failwith("You must first call Server.setup_task_pool")
  };

Flows.reset_timeout := (() => reset_timeout(get()));
Flows.get_state := get_state;
Flows.set_state := set_state;
Flows.get_task_pool := get_task_pool;
