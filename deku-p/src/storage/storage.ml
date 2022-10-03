module Chain = struct
  open Deku_stdlib
  open Deku_chain

  let temp = "chain.tmp.json"
  let file = "chain.json"

  let read ~env:_ ~folder =
    let file = Filename.concat folder file in
    match IO.file_exists file with
    | true ->
        let chain =
          Eio_unix.run_in_systhread (fun () ->
              let json = Yojson.Safe.from_file file in
              Chain.t_of_yojson json)
        in
        Some chain
    | false -> None

  let write ~env ~folder chain =
    Eio_unix.run_in_systhread (fun () ->
        let temp = Filename.concat folder temp in
        let json = Chain.yojson_of_t chain in
        Yojson.Safe.to_file temp json);
    let fs = Eio.Stdenv.fs env in
    let temp = Eio.Path.(fs / folder / temp) in
    let file = Eio.Path.(fs / folder / file) in
    Eio.Path.rename temp file
end
