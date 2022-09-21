open Deku_stdlib

module Config = struct
  open Deku_crypto

  type config = {
    secret : Secret.t;
    validators : Key_hash.t list;
    nodes : Uri.t list;
  }

  and t = config [@@deriving yojson]

  let file = "config.json"
  let make ~secret ~validators ~nodes = { secret; validators; nodes }

  let read ~folder =
    let file = Filename.concat folder file in
    Lwt_io.with_file ~mode:Input file (fun ic ->
        let%await json = Lwt_io.read ic in
        let json = Yojson.Safe.from_string json in
        let config = config_of_yojson json in
        Lwt.return config)

  let write ~folder storage =
    let file = Filename.concat folder file in
    Lwt_io.with_file ~mode:Output file (fun oc ->
        let json = yojson_of_config storage in
        let string = Yojson.Safe.pretty_to_string json in
        Lwt_io.write oc string)
end

module Chain = struct
  open Deku_chain

  let temp = "chain.tmp.json"
  let file = "chain.json"

  let read ~folder =
    let file = Filename.concat folder file in
    let%await exists = Lwt_unix.file_exists file in

    match exists with
    | true ->
        Lwt_io.with_file ~mode:Input file (fun ic ->
            let%await json = Lwt_io.read ic in
            let json = Yojson.Safe.from_string json in
            let chain = Chain.t_of_yojson json in
            Lwt.return (Some chain))
    | false -> Lwt.return_none

  let write ~pool ~folder (chain : Chain.t) =
    let temp = Filename.concat folder temp in
    let file = Filename.concat folder file in
    let%await bin =
      Parallel.async pool (fun () ->
          let json = Chain.yojson_of_t chain in
          Yojson.Safe.to_string json)
    in
    let%await () =
      Lwt_io.with_file ~mode:Output temp (fun oc ->
          let%await () = Lwt_io.write oc bin in
          Lwt_io.close oc)
    in
    Lwt_unix.rename temp file
end
