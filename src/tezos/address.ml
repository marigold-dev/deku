open Deku_repr
open Deku_crypto
open Deku_stdlib

type address =
  | Implicit of Key_hash.t
  | Originated of { contract : Contract_hash.t; entrypoint : string option }

and t = address [@@deriving eq, ord, yojson]

include With_encodings_of_many (struct
  type t = address

  let to_b58 address =
    match address with
    | Implicit key_hash -> Key_hash.to_b58 key_hash
    | Originated { contract; entrypoint = None } ->
        Contract_hash.to_b58 contract
    | Originated { contract; entrypoint = Some entrypoint } ->
        Contract_hash.to_b58 contract ^ "%" ^ entrypoint

  let of_b58 =
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
      let%some contract = Contract_hash.of_b58 contract in
      Some (Originated { contract; entrypoint })
    in
    [ implicit; originated ]
end)

let contract_encoding =
  let open Data_encoding in
  def "contract_id" ~title:"A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. Can be a \
       base58 implicit contract hash or a base58 originated contract hash."
  @@ union ~tag_size:`Uint8
       [
         case (Tag 0) ~title:"Implicit" Key_hash.encoding
           (function Implicit k -> Some k | _ -> None)
           (fun k -> Implicit k);
         case (Tag 1)
           (Fixed.add_padding Contract_hash.encoding 1)
           ~title:"Originated"
           (function Originated { contract; _ } -> Some contract | _ -> None)
           (fun contract -> Originated { contract; entrypoint = None });
       ]

include With_encoding_of_b58 (struct
  open Data_encoding

  type t = address

  let name = "address"
  let title = "An contract address optionally followed by an entrypoint."

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
      (tup2 contract_encoding Variable.string)

  let of_b58 = of_b58
  let to_b58 = to_b58
end)
