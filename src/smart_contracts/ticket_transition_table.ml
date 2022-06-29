open Helpers

module Errors = struct
  type t =
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount
    | `Attempted_to_merge_different_tickets ]
  [@@deriving show]

  let doesnt_exist = Error `Ticket_doesnt_exist

  let ownership_violation = Error `Ticket_ownership_violation

  let ticket_split_error = Error `Ticket_split_invalid_amount
end

module type S = sig
  include Conversions.S

  module Ticket_handle :
    Ticket_handle.S
      with module Address = Address
       and module Amount = Amount
       and module Ticket_id = Ticket_id

  type t

  val own :
    t ->
    Address.t ->
    Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val mint_ticket :
    t -> sender:Address.t -> amount:Amount.t -> bytes -> Ticket_handle.t

  val read_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    ( Ticket_id.t * Amount.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val split_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    amounts:Amount.t * Amount.t ->
    ( Ticket_handle.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist
      | `Ticket_ownership_violation
      | `Ticket_split_invalid_amount ] )
    result

  val join_tickets :
    t ->
    sender:Address.t ->
    handles:Ticket_handle.t * Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val init :
    self:Address.t ->
    tickets:(Ticket_id.t * Amount.t) Seq.t ->
    temporary_tickets:(Ticket_id.t * Amount.t) Seq.t ->
    t

  val finalize : t -> (Ticket_id.t * Amount.t) Seq.t
end

module Make
    (CC : Conversions.S)
    (Ticket_handle : Ticket_handle.S
                       with module Address = CC.Address
                        and module Amount = CC.Amount
                        and module Ticket_id = CC.Ticket_id) =
struct
  module Ticket_handle = Ticket_handle

  module Table = Hashtbl.Make (struct
    include Int64

    let hash t = Int64.to_int t
  end)

  include CC

  module Owner = struct
    type t =
      | Owned         of Address.t
      | To_be_dropped
    [@@deriving yojson]
  end

  open struct
    let ok = Ok ()

    let assert_available sender = function
      | Owner.To_be_dropped -> ok
      | Owned addr when Address.equal addr sender -> ok
      | Owned _ -> Errors.ownership_violation

    let assert_valid_amount ticket_total amount =
      if Amount.(equal ticket_total amount) then
        Ok ()
      else
        Errors.ticket_split_error
  end

  module Ticket_repr = struct
    type t = {
      ticket : Ticket_id.t;
      amount : Amount.t;
      owner : Owner.t;
    }
    [@@deriving yojson]

    let make ticket amount owner = { ticket; amount; owner }

    let join t1 t2 =
      { t1 with amount = Amount.(t1.amount + t2.amount); owner = To_be_dropped }

    let split ticket ~ticket_total ~amounts:(first, second) =
      let total_amount = Amount.(first + second) in
      let%ok () = assert_valid_amount ticket_total total_amount in
      let base = { ticket; owner = To_be_dropped; amount = Amount.zero } in
      let ticket1 = { base with amount = first } in
      let ticket2 = { base with amount = second } in
      Ok (ticket1, ticket2)
  end

  type t = {
    mutable counter : Int64.t;
    table : Ticket_repr.t Table.t;
  }

  let incr t =
    let counter = Int64.succ t.counter in
    t.counter <- counter;
    counter

  let merge t ~handle ~repr = Table.add t.table handle repr

  let unsafe_read { table; counter = _ } ~handle =
    Table.find_opt table handle
    |> Option.fold
         ~some:(fun x ->
           Table.remove table handle;
           Ok x)
         ~none:Errors.doesnt_exist

  let own t sender ticket_handle =
    let%ok ticket = unsafe_read t ~handle:ticket_handle in
    let%ok () = assert_available sender ticket.Ticket_repr.owner in
    let ticket = { ticket with owner = Owned sender } in
    let handle = incr t in
    let () = merge t ~handle ~repr:ticket in
    Ok handle

  let mint_ticket t ~sender ~amount data =
    let ticket = Ticket_id.mint_ticket ~contract_address:sender ~data in
    let repr = Ticket_repr.make ticket amount To_be_dropped in
    let handle = incr t in
    let () = merge t ~handle ~repr in
    handle

  let read_ticket t ~sender ~ticket_handle =
    Table.find_opt t.table ticket_handle
    |> Option.fold
         ~some:(fun ({ Ticket_repr.ticket; amount; owner } as repr) ->
           let%ok () = assert_available sender owner in
           let () = Table.remove t.table ticket_handle in
           let ticket_repr = { repr with owner = To_be_dropped } in
           let handle = incr t in
           Table.add t.table handle ticket_repr;
           let to_return = (ticket, amount, handle) in
           Ok to_return)
         ~none:Errors.doesnt_exist

  let split_ticket t ~sender ~ticket_handle ~amounts =
    let%ok { Ticket_repr.ticket; amount; owner } =
      unsafe_read t ~handle:ticket_handle in
    let%ok () = assert_available sender owner in
    let%ok repr, repr2 =
      Ticket_repr.split ticket ~ticket_total:amount ~amounts in
    let handle1 = incr t in
    let handle2 = incr t in
    let () = merge t ~handle:handle1 ~repr in
    let () = merge t ~handle:handle2 ~repr:repr2 in
    Ok (handle1, handle2)

  let join_tickets t ~sender ~handles:(handle1, handle2) =
    let%ok repr = unsafe_read t ~handle:handle1 in
    let%ok repr2 = unsafe_read t ~handle:handle2 in
    let%ok () = assert_available sender repr.Ticket_repr.owner in
    let%ok () = assert_available sender repr2.Ticket_repr.owner in
    let repr = Ticket_repr.join repr repr2 in
    let handle1 = incr t in
    let () = merge t ~handle:handle1 ~repr in
    Ok handle1

  let init ~self ~tickets ~temporary_tickets =
    let temporary_tickets =
      Seq.map
        (fun (ticket, amount) ->
          Ticket_repr.make ticket amount Owner.To_be_dropped)
        temporary_tickets in
    let tickets =
      Seq.map
        (fun (ticket, amount) ->
          Ticket_repr.make ticket amount (Owner.Owned self))
        tickets in
    let acc = { counter = Int64.of_int (-1); table = Table.create 25 } in
    Seq.iter
      (fun (x : Ticket_repr.t) ->
        let ticket_handle = incr acc in
        Table.add acc.table ticket_handle x)
      (Seq.append temporary_tickets tickets);
    acc

  let finalize t =
    Table.fold
      (fun _ (data : Ticket_repr.t) acc ->
        match data with
        | { owner = Owned _; ticket; amount } -> Seq.cons (ticket, amount) acc
        | { owner = To_be_dropped; ticket = _; amount = _ } -> acc)
      t.table Seq.empty
end
