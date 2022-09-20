open Deku_stdlib

module Config = struct
  open Deku_crypto

  type config = {
    secret : Secret.t;
    validators : Key_hash.t list;
    nodes : Uri.t list;
  }

  and t = config [@@deriving yojson]

  let make ~secret ~validators ~nodes = { secret; validators; nodes }

  let read ~file =
    Lwt_io.with_file ~mode:Input file (fun ic ->
        let%await json = Lwt_io.read ic in
        let json = Yojson.Safe.from_string json in
        let config = config_of_yojson json in
        Lwt.return config)

  let write ~file storage =
    Lwt_io.with_file ~mode:Output file (fun oc ->
        let json = yojson_of_config storage in
        let string = Yojson.Safe.pretty_to_string json in
        Lwt_io.write oc string)
end

module Chain = struct
  open Deku_chain

  let read ~file =
    let%await exists = Lwt_unix.file_exists file in
    match exists with
    | true ->
        Lwt_io.with_file ~mode:Input file (fun ic ->
            let%await (chain : Chain.t) = Lwt_io.read_value ic in
            Lwt.return (Some chain))
    | false -> Lwt.return_none

  let write ~pool ~file (chain : Chain.t) =
    let%await temp, oc =
      Lwt_io.open_temp_file ~prefix:"chain" ~suffix:"bin" ()
    in
    let%await bin =
      Parallel.async pool (fun () -> Marshal.to_string chain [])
    in
    (* TODO: if those fails it will leak temp_files *)
    let%await () = Lwt_io.write oc bin in
    let%await () = Lwt_io.close oc in
    Lwt_unix.rename temp file
end
