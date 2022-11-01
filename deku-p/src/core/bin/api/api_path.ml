open Deku_concepts
open Deku_consensus
open Deku_stdlib

module Level_or_hash = struct
  type t = Level of Level.t | Hash of Block_hash.t

  let parser path =
    let serialize data =
      match data with
      | Level level -> Level.show level
      | Hash hash -> Block_hash.to_b58 hash
    in
    let parse string =
      let parse_level string =
        try
          string |> Z.of_string |> N.of_z |> Option.map Level.of_n
          |> Option.map (fun level -> Level level)
        with _ -> None
      in
      let parse_hash string =
        string |> Block_hash.of_b58 |> Option.map (fun hash -> Hash hash)
      in
      match (parse_level string, parse_hash string) with
      | None, None -> None
      | Some level, _ -> Some level
      | _, Some hash -> Some hash
    in
    Routes.custom ~serialize ~parse ~label:":level-or-hash" path
end

module Operation_hash = struct
  open Deku_protocol

  type t = Operation_hash.t

  let parser path =
    let serialize hash = Operation_hash.to_b58 hash in
    let parse string = Operation_hash.of_b58 string in
    Routes.custom ~serialize ~parse ~label:":operation-hash" path
end

module Address = struct
  type t = Address

  let parser path =
    let open Deku_ledger in
    let serialize address = Address.to_b58 address in
    let parse string = Address.of_b58 string in
    Routes.custom ~serialize ~parse ~label:":address" path
end

module Contract_address = struct
  open Deku_ledger

  type t = Contract_address.t

  let parser path =
    let open Deku_ledger in
    let serialize address = Contract_address.to_b58 address in
    let parse string = Contract_address.of_b58 string in
    Routes.custom ~serialize ~parse ~label:":contract_address" path
end

module Ticketer = struct
  type t = Deku_ledger.Ticket_id.ticketer

  open Deku_ledger.Ticket_id

  let parser path =
    let serialize ticket_id =
      match ticket_id with
      | Tezos contract_hash -> Deku_tezos.Contract_hash.to_b58 contract_hash
      | Deku contract_address ->
          Deku_ledger.Contract_address.to_b58 contract_address
    in
    let parse ticketer =
      Deku_repr.decode_variant
        [
          (fun x ->
            Deku_tezos.Contract_hash.of_b58 x
            |> Option.map (fun x -> Deku_ledger.Ticket_id.Tezos x));
          (fun x ->
            Deku_ledger.Contract_address.of_b58 x
            |> Option.map (fun x -> Deku_ledger.Ticket_id.Deku x));
        ]
        ticketer
    in
    Routes.custom ~serialize ~parse ~label:":ticketer" path
end

module Data = struct
  type t = bytes

  let parser path =
    let serialize data = data |> Bytes.to_string |> Hex.of_string |> Hex.show in
    let parse string =
      let string =
        match String.starts_with ~prefix:"0x" string with
        | false -> string
        | true -> String.sub string 2 (String.length string - 2)
      in
      Hex.to_string (`Hex string) |> Bytes.of_string |> Option.some
    in
    Routes.custom ~serialize ~parse ~label:":data" path
end
