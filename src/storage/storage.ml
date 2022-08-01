open Deku_stdlib
open Deku_crypto

type storage = {
  secret : Secret.t; (* bootstrap *)
  initial_validators : Key_hash.t list;
  nodes : Uri.t list;
}

and t = storage [@@deriving yojson]

let read () =
  let open Lwt.Infix in
  (* TODO: this should not be hard coded *)
  let file = "storage.json" in
  Lwt_io.with_file ~mode:Input file (fun ic ->
      Lwt_io.read ic >|= fun json ->
      let json = Yojson.Safe.from_string json in
      storage_of_yojson json)
