open Helpers

let main =
  let%await () = Repository.init () in
  Interval.run ()

let _ = Lwt_main.run main