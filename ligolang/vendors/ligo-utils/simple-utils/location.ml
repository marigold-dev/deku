(* type file_location = { *)
(*   filename : string ; *)
(*   start_line : int ; *)
(*   start_column : int ; *)
(*   end_line : int ; *)
(*   end_column : int ; *)
(* } *)

type virtual_location = string

type t =
  | File of Region.t (* file_location *)
  | Virtual of virtual_location

let to_yojson = function
  | File reg  -> `List [`String "File"; Region.to_yojson reg]
  | Virtual v -> `List [`String "Virtual"; `String v]
let of_yojson = function
  | `List [`String "File"; reg] ->
    let reg = Region.of_yojson reg in
    (match reg with
    | Ok reg -> Ok (File reg)
    | _ ->
      Utils.error_yojson_format "File Region.t"
    )
  | `List [`String "Virtual"; `String v] ->
    Ok (Virtual v)
  | _ ->
    Utils.error_yojson_format "File Region.t | Virtual String"

let pp = fun ppf t ->
  match t with
  | Virtual _s -> Format.fprintf ppf ""
  | File f -> Format.fprintf ppf "%s" (f#to_string `Point)

let compare a b = match a,b with
  | (File a, File b) -> Region.compare a b
  | (File _, Virtual _) -> -1
  | (Virtual _, File _) -> 1
  | (Virtual a, Virtual b) -> String.compare a b


let make (start_pos:Lexing.position) (end_pos:Lexing.position) : t =
  File (Region.make ~start:(Pos.from_byte start_pos)
                    ~stop:(Pos.from_byte end_pos))

let virtual_location s = Virtual s
let dummy = virtual_location "dummy"
let generated = virtual_location "generated"

let is_dummy_or_generated = function
  | Virtual "dummy" | Virtual "generated" -> true
  | _ -> false

type 'a wrap = {
  wrap_content : 'a ;
  location : t ;
}

let wrap_to_yojson f {wrap_content;location} =
  `Assoc [("wrap_content", f wrap_content); ("location",to_yojson location)]
let wrap_of_yojson f = function
  | `Assoc [("wrap_content", wrap_content); ("location",location)] ->
    let wrap_content = f wrap_content in
    let location = of_yojson location in
    (match (wrap_content,location) with
    | Ok wrap_content, Ok location ->
      Ok {wrap_content;location}
    | _ ->
     Utils.error_yojson_format "{wrap_content: 'a; location: location}"
     )
  | _ ->
     Utils.error_yojson_format "{wrap_content: 'a; location: location}"


let compare_wrap ~compare:compare_content { wrap_content = wca ; location = la } { wrap_content = wcb ; location = lb } =
  match compare_content wca wcb with
  | 0 -> compare la lb
  | c -> c

let compare_content ~compare:compare_content wa wb =
  compare_content wa.wrap_content wb.wrap_content

let equal_content ~equal:equal_content wa wb =
  equal_content wa.wrap_content wb.wrap_content

let wrap ?(loc = generated) wrap_content = { wrap_content ; location = loc }
let get_location x = x.location
let unwrap { wrap_content ; _ } = wrap_content
let map f x = { x with wrap_content = f x.wrap_content }
let fold f acc x = f acc x.wrap_content
let fold_map f acc x = 
  let acc,wrap_content = f acc x.wrap_content in
  acc,{ x with wrap_content}

let pp_wrap f ppf { wrap_content ; _ } = Format.fprintf ppf "%a" f wrap_content

let lift_region : 'a Region.reg -> 'a wrap = fun x ->
  wrap ~loc:(File x.region) x.value
let lift : Region.region -> t = fun x -> File x
let pp_lift = fun ppf r -> pp ppf @@ lift r

let r_extract : 'a Region.reg -> t = fun x -> File x.region
let r_split : 'a Region.reg -> ('a * t) = fun x -> x.value , File x.region
let cover : t -> t -> t = fun a b ->
  match a , b with
  | File _ , Virtual _ -> a
  | Virtual _ , _ -> b
  | File rega , File regb -> File (Region.cover rega regb)

let get_file : t -> Region.t option = function
  | File r -> Some r
  | _ -> None
