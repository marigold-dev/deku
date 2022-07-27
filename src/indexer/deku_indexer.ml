open Cmdliner
open Helpers

let main uri port initial_height database_uri =
  let module Parameters = struct
    let node_uri = uri

    let port = port
  end in
  let%await () = Repository.init ~uri:database_uri initial_height in
  let%await _ =
    Lwt.both (Interval.run uri) (Webserver.run (module Parameters)) in
  await (`Ok ())

let run =
  let node_uri =
    let uri =
      let parser uri = Ok (uri |> Uri.of_string) in
      let printer ppf uri = Format.fprintf ppf "%s" (uri |> Uri.to_string) in
      let open Arg in
      conv (parser, printer) in

    let docv = "uri" in
    let doc = "The node uri you want to pull blocks from" in
    let env = Cmd.Env.info "NODE_URI" in
    let default = Uri.of_string "http://localhost:4440" in
    let open Arg in
    value & opt uri default & info ["node-uri"] ~docv ~doc ~env in
  let port =
    let port =
      let parser string =
        int_of_string_opt string
        |> Option.to_result ~none:(`Msg "Cannot parse port") in
      let printer ppf port = Format.fprintf ppf "%s" (string_of_int port) in
      let open Arg in
      conv (parser, printer) in
    let docv = "port" in
    let doc = "The port on which you want the webserver to run" in
    let env = Cmd.Env.info "PORT" in
    let default = 8080 in
    let open Arg in
    value & opt port default & info ["port"] ~docv ~doc ~env in
  let initial_height =
    let docv = "initial_height" in
    let doc = "The height to start querying at" in
    let env = Cmd.Env.info "INITIAL_HEIGHT" in
    let default = 0L in
    let open Arg in
    value & opt int64 default & info ["initial-height"] ~docv ~doc ~env in
  let database_uri =
    let open Arg in
    let docv = "database uri" in
    let doc = "The uri to sqlite3 database" in
    let env = Cmd.Env.info "SQLITE3_DATABASE_URI" in
    let default = "sqlite3:database.db" |> Uri.of_string in
    let database_uri =
      let parser string = string |> Uri.of_string |> Result.ok in
      let printer ppf database_uri =
        Format.fprintf ppf "%s" (Uri.to_string database_uri) in
      conv (parser, printer) in
    value & opt database_uri default & info ["database-uri"] ~docv ~doc ~env
  in
  let open Term in
  ret
    (const Lwt_main.run
    $ (const main $ node_uri $ port $ initial_height $ database_uri))

let info =
  let doc = "Deku indexer" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-indexer" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-indexer"

let _ = Cmd.eval (Cmd.v info run)
