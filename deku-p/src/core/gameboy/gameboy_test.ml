open Deku_gameboy
open Deku_stdlib

let main () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Deku_gameboy.init net;
  while true do
    Unix.sleep 1;
    let state = Deku_gameboy.send_input (Input (Some Joypad.A)) in
    Logs.info (fun m -> m "Current cycle state: %a" N.pp state)
  done

let () = main ()
