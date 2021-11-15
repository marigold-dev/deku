include Tezos_error_monad.Error_monad
include Tezos_utils.Error_monad
open Memory_proto_alpha

let (>>??) = Alpha_environment.Error_monad.(>>?)

let alpha_wrap a = Alpha_environment.wrap_tzresult a
let alpha_error_wrap x = Memory_proto_alpha.Alpha_environment.Ecoproto_error x

let force_ok_alpha ~msg a = force_ok ~msg @@ alpha_wrap a

let force_lwt ~msg a = force_ok ~msg @@ Lwt_main.run a

let force_lwt_alpha ~msg a = force_ok ~msg @@ alpha_wrap @@ Lwt_main.run a

let assert_error res =
  res >>= function Ok _ -> assert false | Error _ -> return_unit

let (>>=??) a f =
  a >>= fun a ->
  match alpha_wrap a with
  | Ok result -> f result
  | Error errs -> Lwt.return (Error errs)


