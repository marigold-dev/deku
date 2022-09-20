open Deku_stdlib

module Chain = struct
  open Deku_chain

  let temp = "chain.tmp.bin"
  let file = "chain.bin"

  let read ~folder =
    let file = Filename.concat folder file in
    let%await exists = Lwt_unix.file_exists file in

    match exists with
    | true ->
        Lwt_io.with_file ~mode:Input file (fun ic ->
            let%await (chain : Chain.t) = Lwt_io.read_value ic in
            Lwt.return (Some chain))
    | false -> Lwt.return_none

  let write ~pool ~folder (chain : Chain.t) =
    let temp = Filename.concat folder temp in
    let file = Filename.concat folder file in
    let%await bin =
      Parallel.async pool (fun () -> Marshal.to_string chain [])
    in
    let%await () =
      Lwt_io.with_file ~mode:Output temp (fun oc ->
          let%await () = Lwt_io.write oc bin in
          Lwt_io.close oc)
    in
    Lwt_unix.rename temp file
end
