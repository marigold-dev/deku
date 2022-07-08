open Cmdliner
open Helpers

let main uri =
  let module Parameters = struct
    let node_uri = uri
  end in
  let%await () = Repository.init () in
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
  let open Term in
  ret (const Lwt_main.run $ (const main $ node_uri))

let info =
  let doc = "Deku indexer" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-indexer" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-indexer"

let _ = Cmd.eval (Cmd.v info run)
