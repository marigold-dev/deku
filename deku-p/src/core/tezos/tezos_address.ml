open Deku_stdlib

type tezos_address =
  | Tezos_address of { contract : Tezos_contract.t; entrypoint : string }

and t = tezos_address [@@deriving show, eq, ord]

let make ~contract ~entrypoint = Tezos_address { contract; entrypoint }

let to_b58 = function
  | Tezos_address { contract; entrypoint = "default" } ->
      Tezos_contract.to_b58 contract
  | Tezos_address { contract; entrypoint } ->
      Tezos_contract.to_b58 contract ^ "%" ^ entrypoint

let of_b58 string =
  let%some contract, entrypoint =
    match String.split_on_char '%' string with
    | [ contract ] -> Some (contract, "")
    | [ contract; entrypoint ]
      when String.length entrypoint < 32 && entrypoint <> "default" ->
        Some (contract, entrypoint)
    | _ -> None
  in
  let%some contract = Tezos_contract.of_b58 contract in
  Some (Tezos_address { contract; entrypoint })

let encoding =
  let open Data_encoding in
  let name = "address" in
  let raw_encoding =
    conv_with_guard
      (fun address ->
        let (Tezos_address { contract; entrypoint }) = address in
        (contract, entrypoint))
      (fun (contract, entrypoint) ->
        match entrypoint with
        | entrypoint when String.length entrypoint > 31 ->
            Error "Entrypoint name too long"
        | "default" -> Ok (Tezos_address { contract; entrypoint = "" })
        | entrypoint -> Ok (Tezos_address { contract; entrypoint }))
      (tup2 Tezos_contract.encoding Variable.string)
  in
  Deku_repr.make_encoding ~name ~to_string:to_b58 ~of_string:of_b58
    ~raw_encoding

let cmdliner_converter =
  let of_string s =
    match of_b58 s with
    | Some s -> `Ok s
    | None ->
        `Error (Format.sprintf "Could not parse '%s' as a Tezos address" s)
  in
  let to_string fmt address = Format.fprintf fmt "%s" (to_b58 address) in
  (of_string, to_string)
