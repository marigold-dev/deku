open Deku_stdlib

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
            let chain = Chain.chain_data_of_yojson json in
            Lwt.return (Some chain))
    | false -> Lwt.return_none

  let write ~pool ~folder (chain_data : Chain.chain_data) =
    let temp = Filename.concat folder temp in
    let file = Filename.concat folder file in
    let%await bin =
      Parallel.async pool (fun () ->
          let json = Chain.yojson_of_chain_data chain_data in
          Yojson.Safe.to_string json)
    in
    let%await () =
      Lwt_io.with_file ~mode:Output temp (fun oc ->
          let%await () = Lwt_io.write oc bin in
          Lwt_io.close oc)
    in
    Lwt_unix.rename temp file
end
