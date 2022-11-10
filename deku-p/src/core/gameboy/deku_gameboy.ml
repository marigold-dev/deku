open Camlboy_lib
module Camlboy = Camlboy.Make (Mbc1)

type gameboy_state = Camlboy.t

let yojson_of_gameboy_state t =
  let str = Marshal.to_string t [] in
  `String str

let gameboy_state_of_yojson json : gameboy_state =
  match json with
  | `String str -> Marshal.from_string str 0
  | _ -> failwith "invalid gameboy state json"

type t = gameboy_state option [@@deriving yojson]

let empty = None

type frame_buffer = [ `White | `Light_gray | `Dark_gray | `Black ] array array
[@@deriving ord, eq, yojson, show]

let show t = Camlboy.show @@ Option.get t

let init ~rom_path =
  let rom_bytes = Read_rom_file.f rom_path in
  Some (Camlboy.create_with_rom ~rom_bytes ~print_serial_port:false)

let rec advance t : frame_buffer =
  match t with
  | Some t -> (
      match Camlboy.run_instruction t with
      | Camlboy_lib.Gpu.In_frame -> advance (Some t)
      | Frame_ended frame_buffer -> frame_buffer)
  | None ->
      failwith "You must initialize the gameboy state before calling advance."
