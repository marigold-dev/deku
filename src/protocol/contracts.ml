open Deku_stdlib
open Deku_concepts

module Conversions :
  Smart_contracts.Conversions
    with type Address.t = Address.t
     and type Amount.t = Amount.t
     and type Ticket_id.t = Ticket_id.t = struct
  open Core

  module Address = struct
    include Address

    let size = 36
    let to_bytes t = Bytes.of_string (to_b58 t)

    let of_bytes t =
      let addr = Address.of_b58 (Bytes.to_string t) in
      Option.value_exn addr
  end

  module Ticket_id = struct
    include Ticket_id

    let size t =
      let (Ticket_id { ticketer = _; data }) = t in
      Address.size + Bytes.length data

    let to_bytes t =
      let (Ticket_id { ticketer; data }) = t in
      let byt =
        match ticketer with
        | Deku t -> Address.to_b58 t
        | Tezos t -> Deku_tezos.Contract_hash.to_string t
      in
      let byt = Bytes.of_string byt in
      let byt2 = data in
      Stdlib.Bytes.cat byt byt2

    let to_string t =
      let (Ticket_id { ticketer; data }) = t in
      let loc =
        let open Tezos_micheline.Micheline_printer in
        { comment = None }
      in
      let ticketer =
        match ticketer with
        | Deku t -> Address.to_b58 t
        | Tezos t -> Deku_tezos.Contract_hash.to_string t
      in
      let micheline =
        Tezos_micheline.Micheline.Prim
          (loc, "Pair", [ String (loc, ticketer); Bytes (loc, data) ], [])
      in
      Format.asprintf "%a" Tezos_micheline.Micheline_printer.print_expr
        micheline

    let mint_ticket ~contract_address ~data = mint_ticket contract_address data
  end

  module Amount = struct
    include Amount

    let size = 8

    let of_int t =
      t |> Z.of_int |> N.of_z
      |> Stdlib.Option.map Amount.of_n
      |> Option.value ~default:Amount.zero

    let to_int t = t |> Amount.to_n |> N.to_z |> Z.to_int
  end
end

module Context :
  Smart_contracts.CTX
    with module Address = Conversions.Address
     and module Ticket_id = Conversions.Ticket_id
     and module Amount = Conversions.Amount =
  Smart_contracts.Make_ffi (Conversions)

module Contract_vm = Smart_contracts.Contract_vm.Make (Context)
