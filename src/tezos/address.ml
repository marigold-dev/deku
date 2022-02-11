open Crypto
open Helpers
type t =
  | Implicit   of Key_hash.t
  | Originated of {
      contract : Contract_hash.t;
      entrypoint : string option;
    }
[@@deriving eq, ord]
let to_string = function
  | Implicit key_hash -> Key_hash.to_string key_hash
  | Originated { contract; entrypoint = None } ->
    Contract_hash.to_string contract
  | Originated { contract; entrypoint = Some entrypoint } ->
    Contract_hash.to_string contract ^ "%" ^ entrypoint
let of_string =
  let implicit string =
    let%some implicit = Key_hash.of_string string in
    Some (Implicit implicit) in
  let originated string =
    let%some contract, entrypoint =
      match String.split_on_char '%' string with
      | [contract] -> Some (contract, None)
      | [contract; entrypoint]
        when String.length entrypoint < 32 && entrypoint <> "default" ->
        Some (contract, Some entrypoint)
      | _ -> None in
    let%some contract = Contract_hash.of_string contract in
    Some (Originated { contract; entrypoint }) in
  Encoding_helpers.parse_string_variant [implicit; originated]

let contract_encoding =
  let open Data_encoding in
  def "contract_id" ~title:"A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. Can be a \
       base58 implicit contract hash or a base58 originated contract hash."
  @@ union ~tag_size:`Uint8
       [
         case (Tag 0) ~title:"Implicit" Key_hash.encoding
           (function
             | Implicit k -> Some k
             | _ -> None)
           (fun k -> Implicit k);
         case (Tag 1)
           (Fixed.add_padding Contract_hash.encoding 1)
           ~title:"Originated"
           (function
             | Originated { contract; _ } -> Some contract
             | _ -> None)
           (fun contract -> Originated { contract; entrypoint = None });
       ]
let encoding =
  let open Data_encoding in
  let name = "address" in
  let title = "An contract address optionally followed by an entrypoint." in
  let raw_encoding =
    conv
      (fun t ->
        match t with
        | Implicit _ as t -> (t, "")
        | Originated { contract = _; entrypoint } as t ->
          let entrypoint = Option.value ~default:"" entrypoint in
          (t, entrypoint))
      (function
        | (Implicit _ as t), _ -> t
        | Originated { contract; _ }, ("" | "default") ->
          Originated { contract; entrypoint = None }
        | Originated { contract; _ }, entrypoint ->
          Originated { contract; entrypoint = Some entrypoint })
      (tup2 contract_encoding Variable.string) in
  Encoding_helpers.make_encoding ~name ~title ~to_string ~of_string
    ~raw_encoding
let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "address" to_string of_string
