open Craft_protocol

(* TODO: I just wrote this number down, have no idea if it makes sense, magic *)
let backlog = 1024

(* TODO: this can be much smaller - check client.h  *)
let max_size = 128 * 1024 * 1024
let port = 4080

type client = { id : int; write : Server_commands.t -> unit }

let client_index = ref (-1)
let clients = ref []

let add_client write_buf =
  let id = !client_index in
  client_index := id + 1;
  let write command =
    let str = Server_commands.serialize command ^ "\n" in
    Eio.Buf_write.string write_buf str
  in
  let client = { id; write } in
  clients := client :: !clients;
  client

let main () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let interface = Eio.Net.Ipaddr.V4.any in
  let address = `Tcp (interface, port) in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net address
  in

  let state = ref Craft_handler.empty in
  let rec loop () =
    Eio.Net.accept_fork ~sw socket
      ~on_error:(fun err -> Format.eprintf "%s\n%!" @@ Printexc.to_string err)
      (fun stream _sockaddr ->
        Eio.Buf_write.with_flow ~initial_size:max_size stream
        @@ fun write_buf ->
        let client = add_client write_buf in
        let read_buf =
          Eio.Buf_read.of_flow ~initial_size:max_size ~max_size stream
        in
        let broadcast command =
          List.iter
            (fun { id; write } ->
              if not (Int.equal id client.id) then write command)
            !clients
        in
        let lines = Eio.Buf_read.lines read_buf in
        let do_effect = function
          | Craft_handler.Broadcast command -> broadcast command
          | Craft_handler.Respond command ->
              Format.printf "Responding: %a" Server_commands.pp command;
              client.write command
        in
        let new_state, effects =
          Craft_handler.handle_client_connect ~client_id:client.id !state
        in
        List.iter do_effect effects;
        state := new_state;
        Seq.iter
          (fun command ->
            let command = Client_commands.deserialize command in
            Format.printf "Received: %a\n%!" Client_commands.pp command;
            let new_state, effects =
              Craft_handler.handle_client_command ~client_id:client.id !state
                command
            in
            List.iter do_effect effects;
            state := new_state)
          lines);
    loop ()
  in
  loop ()

let () = main ()
