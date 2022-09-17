open Deku_stdlib
open Deku_crypto

type storage = {
  secret : Secret.t;
  (* bootstrap *)
  validators : (Key_hash.t * Uri.t) list;
}

and t = storage [@@deriving yojson]

let make ~secret ~validators = { secret; validators }

let read ~file =
  let open Lwt.Infix in
  Lwt_io.with_file ~mode:Input file (fun ic ->
      Lwt_io.read ic >|= fun json ->
      let json = Yojson.Safe.from_string json in
      storage_of_yojson json)

let write ~file storage =
  Lwt_io.with_file ~mode:Output file (fun oc ->
      let json = yojson_of_storage storage in
      let string = Yojson.Safe.pretty_to_string json in
      Lwt_io.write oc string)
