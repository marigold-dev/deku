open Deku_ledger
open Deku_concepts
open Deku_stdlib

module Account : Rapper.CUSTOM with type t = Address.t = struct
  type t = Address.t

  let t =
    let encode account = Ok (Address.to_b58 account) in
    let decode str =
      Address.of_b58 str
      |> Option.to_result ~none:(Format.sprintf "invalid account: %s" str)
    in
    Caqti_type.(custom ~encode ~decode Caqti_type.string)
end

module Ticketer : Rapper.CUSTOM with type t = Ticket_id.ticketer = struct
  type t = Ticket_id.ticketer

  let t =
    let encode ticketer = Ok (Ticket_id.ticketer_to_string ticketer) in
    let decode str =
      Ticket_id.ticketer_of_string str
      |> Option.to_result ~none:(Format.sprintf "invalid account: %s" str)
    in
    Caqti_type.(custom ~encode ~decode Caqti_type.string)
end

module Amount : Rapper.CUSTOM with type t = Amount.t = struct
  type t = Amount.t

  let t =
    let encode amount = Ok (Amount.to_n amount |> N.to_z |> Z.to_int64) in
    let decode int64 =
      Z.of_int64 int64 |> N.of_z |> Option.map Amount.of_n
      |> Option.to_result
           ~none:(Format.sprintf "invalid amount: %s" (Int64.to_string int64))
    in

    (* TODO: this breaks when n > max_int_64. *)
    Caqti_type.(custom ~encode ~decode Caqti_type.int64)
end
