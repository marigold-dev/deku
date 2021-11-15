open Types

let label = function
  | `List [`String "Label"; `String l] -> Ok (Label l)
  | _ -> Utils.error_yojson_format "Label of string"

let binding f g = function
  | `List [x;y] ->
     begin match f x, g y with
     | Ok x, Ok y -> Some (x,y)
     | _ -> None end
  | _ -> None

let err_bad_format =
  Utils.error_yojson_format
    "A label map, represented as an array [ [string , element] , ... ]."

let bindings f g = function
  | `List xs ->
     begin match Option.all @@ List.map ~f:(binding f g) xs with
     | None -> err_bad_format
     | Some xs -> Ok xs end
  | _ -> err_bad_format

let label_map row_elem m =
  Stdlib.Result.map LMap.of_list (bindings label row_elem m)
