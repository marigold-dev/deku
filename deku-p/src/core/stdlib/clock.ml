(* TODO: I reaaaallly don't like having an env ref like this!
   But the only better way I can think of injecting this deep inside
   the chain is with effects and that would be worse.

   We could transform init to accept a continuation like Parallel.run
   but I don't see why we need to.*)
let env_ref : Eio.Time.clock option ref = ref None
let init env = env_ref := Some env

let sleep_until time =
  match !env_ref with
  | Some env -> Eio.Time.sleep_until env time
  | None -> failwith "You must intialize the Clock module first"

let now () =
  match !env_ref with
  | Some env -> Eio.Time.now env
  | None -> failwith "You must initialize the Clock module first"
