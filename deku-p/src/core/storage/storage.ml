module Chain = struct
  open Deku_stdlib
  open Deku_chain

  let temp = "chain.tmp.json"
  let file = "chain.json"

  let read ~env ~folder =
    let file = Filename.concat folder file in

    match IO.file_exists file with
    | true ->
        let fs = Eio.Stdenv.fs env in
        let file = Eio.Path.(fs / file) in
        Eio.Path.with_open_in file @@ fun source ->
        let buf = Cstruct.create 4096 in
        let rec decoding_loop status =
          let open Data_encoding.Binary in
          match status with
          | Success { result; _ } -> result
          | Await feed ->
              let size = Eio.Flow.read source buf in
              let status = feed (Cstruct.to_bytes ~len:size buf) in
              decoding_loop status
          | Error err -> raise (Read_error err)
        in
        let chain =
          decoding_loop (Data_encoding.Binary.read_stream Chain.encoding)
        in
        Some chain
    | false -> None

  let write ~env ~folder chain =
    let fs = Eio.Stdenv.fs env in
    let file = Eio.Path.(fs / folder / file) in
    let temp = Eio.Path.(fs / folder / temp) in
    let binary = Data_encoding.Binary.to_string_exn Chain.encoding chain in
    Eio.Path.save ~create:(`Or_truncate 0o644) temp binary;
    Eio.Path.rename temp file
end
