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

module Joypad = struct
  type t = Joypad.key = Down | Up | Left | Right | Start | Select | B | A
  [@@deriving show, yojson]

  let of_string = function
    | "Down" -> Down
    | "Up" -> Up
    | "Left" -> Left
    | "Right" -> Right
    | "Start" -> Start
    | "Select" -> Select
    | "B" -> B
    | "A" -> A
    | x ->
        raise @@ Invalid_argument (Format.sprintf "Unknown gameboy input %s" x)

  let cmdliner_converter =
    let of_string s =
      try `Ok (of_string s) with Invalid_argument err -> `Error err
    in
    let to_string fmt t = Format.fprintf fmt "%a" pp t in
    (of_string, to_string)

  let encoding =
    let open Data_encoding in
    union ~tag_size:`Uint8
      [
        case ~title:"Down" (Tag 0)
          (Data_encoding.constant "Down")
          (function Down -> Some () | _ -> None)
          (fun () -> Down);
        case ~title:"Up" (Tag 1)
          (Data_encoding.constant "Up")
          (function Up -> Some () | _ -> None)
          (fun () -> Up);
        case ~title:"Left" (Tag 2)
          (Data_encoding.constant "Left")
          (function Left -> Some () | _ -> None)
          (fun () -> Left);
        case ~title:"Right" (Tag 3)
          (Data_encoding.constant "Right")
          (function Right -> Some () | _ -> None)
          (fun () -> Right);
        case ~title:"Start" (Tag 4)
          (Data_encoding.constant "Start")
          (function Start -> Some () | _ -> None)
          (fun () -> Start);
        case ~title:"Select" (Tag 5)
          (Data_encoding.constant "Select")
          (function Select -> Some () | _ -> None)
          (fun () -> Select);
        case ~title:"A" (Tag 6)
          (Data_encoding.constant "A")
          (function A -> Some () | _ -> None)
          (fun () -> A);
        case ~title:"B" (Tag 7)
          (Data_encoding.constant "B")
          (function B -> Some () | _ -> None)
          (fun () -> B);
      ]
end

let send_input key t =
  match t with
  | Some t ->
      Camlboy.press t key;
      let _ = advance (Some t) in
      Camlboy.release t key
  | None ->
      failwith "You must initialize the gameboy state before calling advance."
