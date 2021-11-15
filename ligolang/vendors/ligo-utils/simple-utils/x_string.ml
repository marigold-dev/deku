type t =
  Standard of string
| Verbatim of string

let standard : string -> t = fun s -> Standard s
let verbatim : string -> t = fun s -> Verbatim s

let pp ppf = function
    Standard s -> Format.fprintf ppf "%S" s
  | Verbatim v -> Format.fprintf ppf "{|%s|}" v

let to_yojson = function
    Standard s -> `List [`String "Standard"; `String s]
  | Verbatim v -> `List [`String "Verbatim"; `String v]
let of_yojson = function
    `List [`String "Standard"; `String s] -> Ok (Standard s)
  | `List [`String "Verbatim"; `String v] -> Ok (Verbatim v)
  | _ -> 
    Utils.error_yojson_format "Standard string | Verbatim string"

let compare ?(compare=String.compare) a b = match a,b with
    (Standard a, Standard b) -> compare a b
  | (Standard _, Verbatim _) -> -1
  | (Verbatim _, Standard _) -> 1
  | (Verbatim a, Verbatim b) -> compare a b

let extract = function
    Standard s -> s
  | Verbatim v -> v
