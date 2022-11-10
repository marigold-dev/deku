open StdLabels
open Tsdl

let gb_w = 160
let gb_h = 144
let scale = 6.
let scaled_gb_w = Float.(of_int gb_w *. scale |> to_int)
let scaled_gb_h = Float.(of_int gb_h *. scale |> to_int)
let sec_per_frame = 1. /. 60.

let or_exit = function
  | Error (`Msg e) ->
      Sdl.log "%s" e;
      exit 1
  | Ok x -> x

let create_renderer () =
  Sdl.init Sdl.Init.(video + events) |> or_exit;
  let window =
    Sdl.create_window ~w:scaled_gb_w ~h:scaled_gb_h "CAMLBOY"
      Sdl.Window.windowed
    |> or_exit
  in
  Sdl.create_renderer window ~index:(-1) |> or_exit

let create_texture renderer =
  Sdl.create_texture renderer Sdl.Pixel.format_rgb888
    Sdl.Texture.access_streaming ~w:gb_w ~h:gb_h
  |> or_exit

let render_framebuffer ~texture ~renderer ~fb =
  let copy_framebuffer_to_pixels fb pixels =
    for y = 0 to gb_h - 1 do
      for x = 0 to gb_w - 1 do
        let index = (y * gb_w) + x in
        match fb.(y).(x) with
        | `White -> pixels.{index} <- 0xE5FBF4l
        | `Light_gray -> pixels.{index} <- 0x97AEB8l
        | `Dark_gray -> pixels.{index} <- 0x61687Dl
        | `Black -> pixels.{index} <- 0x221E31l
      done
    done
  in
  Sdl.lock_texture texture None Bigarray.int32
  |> or_exit
  |> (fun (pixels, _) -> pixels)
  |> copy_framebuffer_to_pixels fb;
  Sdl.unlock_texture texture;
  Sdl.render_copy renderer texture |> or_exit;
  Sdl.render_present renderer

let init () =
  let start_time = ref (Unix.gettimeofday ()) in
  let renderer = create_renderer () in
  let texture = create_texture renderer in
  fun framebuffer ->
    render_framebuffer ~texture ~renderer ~fb:framebuffer;
    let duration = Unix.gettimeofday () -. !start_time in
    if duration < sec_per_frame then Unix.sleepf (sec_per_frame -. duration);
    start_time := Unix.gettimeofday ()
