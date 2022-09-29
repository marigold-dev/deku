module Chain = struct
  open Deku_stdlib
  open Deku_chain

  let temp = "chain.tmp.json"
  let file = "chain.json"

  let read ~env ~folder =
    let cwd = Eio.Stdenv.cwd env in
    match
      let file =
        Filename.concat (Filename.concat (Unix.getcwd ()) folder) file
      in
      IO.file_exists file
    with
    | true ->
        let file = Eio.Path.(cwd / folder / file) in
        let json = Eio.Path.load file in
        let json = Yojson.Safe.from_string json in
        Some (Chain.t_of_yojson json)
    | false -> None

  let write ~env ~folder (chain : Chain.t) =
    let cwd = Eio.Stdenv.cwd env in
    let bin =
      let json = Chain.yojson_of_t chain in
      Yojson.Safe.to_string json
    in
    let temp = Eio.Path.(cwd / folder / temp) in
    let file = Eio.Path.(cwd / folder / file) in
    Eio.Path.save ~create:(`If_missing 0o644) temp bin;
    Eio.Path.rename temp file
end
