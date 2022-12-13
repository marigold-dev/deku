open Deku_stdlib

type state = Bytes.t

let state_encoding = Data_encoding.bytes
let empty = Bytes.empty

module Joypad = struct
  type t = Down | Up | Left | Right | Start | Select | B | A
  [@@deriving show, eq]

  let to_string = function
    | Down -> "Down"
    | Up -> "Up"
    | Left -> "Left"
    | Right -> "Right"
    | Start -> "Start"
    | Select -> "Select"
    | A -> "A"
    | B -> "B"

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
    let to_string fmt t = Format.fprintf fmt "%s" (to_string t) in
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

  let%expect_test "Data_encoding round_trip" =
    List.iter
      (fun key ->
        let serialized = Data_encoding.Binary.to_bytes_exn encoding key in
        let deserialized =
          Data_encoding.Binary.of_bytes_exn encoding serialized
        in
        Format.printf "%a <-> %a\n" Hex.pp (Hex.of_bytes serialized) pp
          deserialized)
      [ Down; Up; Left; Right; Start; Select; A; B ];
    [%expect
      {|
      00 <-> Deku_gameboy.Joypad.Down
      01 <-> Deku_gameboy.Joypad.Up
      02 <-> Deku_gameboy.Joypad.Left
      03 <-> Deku_gameboy.Joypad.Right
      04 <-> Deku_gameboy.Joypad.Start
      05 <-> Deku_gameboy.Joypad.Select
      06 <-> Deku_gameboy.Joypad.A
      07 <-> Deku_gameboy.Joypad.B |}]
end

type command =
  | Input of Joypad.t option
  | Input_and_advance of Joypad.t option * int

let max_size = 128 * 1024 * 1024
let net_ref : Eio.Net.t option ref = ref None
let init net = net_ref := Some net

let send_input command =
  match !net_ref with
  | None -> failwith "You must initialize the network first"
  | Some net ->
      Eio.Switch.run @@ fun sw ->
      let flow =
        Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 2222))
      in
      let command_str =
        match command with
        | Input (Some joypad) ->
            Format.sprintf "Input %s" (Joypad.to_string joypad)
        | Input None -> "Input Empty"
        | Input_and_advance (None, n) ->
            Format.sprintf "Input_and_advance Empty %d" n
        | Input_and_advance (Some joypad, n) ->
            Format.sprintf "Input_and_advance %s %d" (Joypad.to_string joypad) n
      in
      (* Logs.app (fun m -> m "Sending emulator command: '%s'" command_str); *)
      Eio.Flow.copy_string (command_str ^ "\n") flow;
      let response = Eio.Buf_read.of_flow ~max_size flow in
      let response =
        Eio.Buf_read.lines response |> List.of_seq |> String.concat "\n"
      in
      (* Logs.app (fun m -> m "Received emulator response: '%s'" response); *)
      Z.of_string response |> N.of_z |> Option.get
