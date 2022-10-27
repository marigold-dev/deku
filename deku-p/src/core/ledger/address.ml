open Deku_crypto
open Deku_stdlib

type address =
  | Implicit of Key_hash.t
  | Originated of { address : Contract_address.t; entrypoint : string option }

and t = address [@@deriving eq, ord, show]

let of_key_hash key_hash = Implicit key_hash
let to_key_hash = function Implicit x -> Some x | Originated _ -> None

let of_contract_address (addr, entrypoint) =
  Originated { address = addr; entrypoint }

let to_contract_address = function
  | Implicit _ -> None
  | Originated x -> Some (x.address, x.entrypoint)

let of_b58 x =
  let implicit string =
    let%some implicit = Key_hash.of_b58 string in
    Some (Implicit implicit)
  in
  let originated string =
    let%some contract, entrypoint =
      match String.split_on_char '%' string with
      | [ contract ] -> Some (contract, None)
      | [ contract; entrypoint ]
        when String.length entrypoint < 32 && entrypoint <> "default" ->
          Some (contract, Some entrypoint)
      | _ -> None
    in
    let%some address = Contract_address.of_b58 contract in
    Some (Originated { address; entrypoint })
  in
  Deku_repr.decode_variant [ implicit; originated ] x

let to_b58 = function
  | Implicit key_hash -> Key_hash.to_b58 key_hash
  | Originated { address; entrypoint = None } -> Contract_address.to_b58 address
  | Originated { address; entrypoint = Some entrypoint } ->
      Contract_address.to_b58 address ^ "%" ^ entrypoint

let t_of_yojson x =
  match x with
  | `String x -> (
      match of_b58 x with
      | Some x -> x
      | None -> Yojson.json_error "wrong format")
  | _ -> Yojson.json_error "wrong format"

let yojson_of_t x = `String (to_b58 x)

let contract_encoding =
  let open Data_encoding in
  def "address" ~title:"A deku_address"
  @@ union ~tag_size:`Uint8
       [
         case (Tag 0) ~title:"Implicit" Key_hash.encoding
           (function Implicit k -> Some k | _ -> None)
           (fun k -> Implicit k);
         case (Tag 1)
           (Fixed.add_padding Contract_address.encoding 1)
           ~title:"Originated"
           (function Originated { address; _ } -> Some address | _ -> None)
           (fun address -> Originated { address; entrypoint = None });
       ]

let encoding =
  let open Data_encoding in
  let name = "address" in
  let raw_encoding =
    conv
      (fun t ->
        match t with
        | Implicit _ as t -> (t, "")
        | Originated { address = _; entrypoint } as t ->
            let entrypoint = Option.value ~default:"" entrypoint in
            (t, entrypoint))
      (function
        | (Implicit _ as t), _ -> t
        | Originated { address; _ }, ("" | "default") ->
            Originated { address; entrypoint = None }
        | Originated { address; _ }, entrypoint ->
            Originated { address; entrypoint = Some entrypoint })
      (tup2 contract_encoding Variable.string)
  in
  Deku_repr.make_encoding ~name ~to_string:to_b58 ~of_string:of_b58
    ~raw_encoding

module Map = Deku_stdlib.Map.Make (struct
  type nonrec t = t

  let compare = compare_address
  let t_of_yojson = t_of_yojson
  let yojson_of_t = yojson_of_t
end)

let cmdliner_converter =
  let of_string s =
    match of_b58 s with
    | Some s -> `Ok s
    | None -> `Error (Format.sprintf "Could not parse '%s' as a Deku address" s)
  in
  let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
  (of_string, to_string)
