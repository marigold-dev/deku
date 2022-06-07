open Crypto

(* TODO: write custom instances of `bin_read_t`, `bin_write_t` necessary for Pollinate *)
type t = Key.t [@@deriving bin_io]

let compare = Key.compare

let to_string = Key.to_string

let of_string = Key.of_string

let to_yojson = Key.to_yojson

let of_yojson = Key.of_yojson

let bin_read_t = Key.bin_read_t
let bin_writer_t = Key.bin_writer_t
let bin_reader_t = Key.bin_reader_t
let __bin_read_t__ = Key.__bin_read_t__
let bin_write_t = Key.bin_write_t
let __bin_write_t__ = Key.__bin_write_t__
let bin_size_t = Key.bin_size_t
let bin_shape_t = Key.bin_shape_t
let bin_t = Key.bin_t

let of_key secret =
  match secret with
  | Secret.Ed25519 secret -> Key.Ed25519 (Ed25519.Key.of_secret secret)
  | Secret.Secp256k1 secret -> Key.Secp256k1 (Secp256k1.Key.of_secret secret)
  | Secret.P256 secret -> Key.P256 (P256.Key.of_secret secret)

let genesis_key = {|edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd|}

let genesis_key =
  match Secret.of_yojson (`String genesis_key) with
  | Ok key -> key
  | Error error -> failwith error

let genesis_wallet = of_key genesis_key
