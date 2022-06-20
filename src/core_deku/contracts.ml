module Conversions : Smart_contracts.Conversions = struct
  open Core

  module Address = struct
    include Address

    let size = 36

    let to_bytes t = Bytes.of_string (to_string t)

    let of_bytes t =
      let addr = Address.of_string (Bytes.to_string t) in
      Option.value_exn addr
  end

  module Ticket_id = struct
    include Ticket_id

    let size t = Address.size + Bytes.length t.data

    let to_bytes t =
      let byt = Tezos.Address.to_string t.ticketer in
      let byt = Bytes.of_string byt in
      let byt2 = t.data in
      Stdlib.Bytes.cat byt byt2
  end

  module Amount = struct
    include Amount

    let size = 8

    let of_int t = Amount.of_int t

    let to_int t = Amount.to_int t
  end
end

module Context = Smart_contracts.Make_ffi (Conversions)
module Contract_vm = Smart_contracts.Contract_vm
