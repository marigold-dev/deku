open Deku_stdlib
open Deku_tezos

(* TODO: Entrypoint.t *)
type address = Address of { contract : Contract.t; entrypoint : string }
and t = address [@@deriving show, eq, ord]

let of_contract ~contract ~entrypoint = Address { contract; entrypoint }

let of_tezos_address tezos_address =
  let open Tezos_address in
  let (Tezos_address { contract; entrypoint }) = tezos_address in
  let contract = Contract.of_tezos_contract contract in
  of_contract ~contract ~entrypoint

let of_b58 string =
  let%some contract, entrypoint =
    match String.split_on_char '%' string with
    | [ contract ] -> Some (contract, "")
    | [ contract; entrypoint ]
      when String.length entrypoint < 32 && entrypoint <> "default" ->
        Some (contract, entrypoint)
    | _ -> None
  in
  let%some contract = Contract.of_b58 contract in
  Some (Address { contract; entrypoint })

let to_b58 address =
  let (Address { contract; entrypoint }) = address in
  match entrypoint with
  | "" -> Contract.to_b58 contract
  | entrypoint -> Contract.to_b58 contract ^ "%" ^ entrypoint

let encoding =
  let open Data_encoding in
  let name = "address" in
  let raw_encoding =
    conv_with_guard
      (fun address ->
        let (Address { contract; entrypoint }) = address in
        (contract, entrypoint))
      (fun (contract, entrypoint) ->
        match entrypoint with
        | entrypoint when String.length entrypoint > 31 ->
            Error "Entrypoint name too long"
        | "default" -> Ok (Address { contract; entrypoint = "" })
        | entrypoint -> Ok (Address { contract; entrypoint }))
      (tup2 Contract.encoding Variable.string)
  in
  Deku_repr.make_encoding ~name ~to_string:to_b58 ~of_string:of_b58
    ~raw_encoding
