open Deku_stdlib

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

  type t

  val own :
    t ->
    Address.t ->
    Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation ] )
    result

  val mint_ticket :
    t -> sender:Address.t -> amount:Amount.t -> bytes -> Ticket_handle.t

  val read_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    ( Ticket_id.t * Amount.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation ] )
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
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation ] )
    result

  val init :
    self:Address.t ->
    mapping:((Ticket_id.t * Amount.t) * Ticket_handle.t) list ->
    tickets:(Ticket_id.t * Amount.t) Seq.t ->
    temporary_tickets:
      ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) Seq.t ->
    t * (Int64.t option * Ticket_handle.t) list option

  val finalize : t -> (Ticket_handle.t * Ticket_id.t * Amount.t) Seq.t
end

open Core

module Make (CC : Conversions.S) = struct
  module Ticket_handle = Ticket_handle
  include CC

  module Owner = struct
    type t = Owned of Address.t | To_be_dropped [@@deriving yojson]
  end

  open struct
    let ok = Ok ()

    let assert_available sender = function
      | Owner.To_be_dropped -> ok
      | Owned addr when Address.equal addr sender -> ok
      | Owned _ -> Errors.ownership_violation

    let assert_valid_amount ticket_total amount =
      if Amount.(equal ticket_total amount) then Ok ()
      else Errors.ticket_split_error
  end

  module Ticket_repr = struct
    type t = { ticket : Ticket_id.t; amount : Amount.t; owner : Owner.t }
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
    mutable counter : Int32.t Seq.t;
    mutable table : Ticket_repr.t Int32.Map.t;
  }

  module Map = Int32.Map

  let incr t =
    let counter, seq = Seq.uncons t.counter |> Option.value_exn in
    t.counter <- seq;
    counter

  let merge t ~handle ~repr =
    let table = Map.set ~key:handle ~data:repr t.table in
    t.table <- table

  let unsafe_read t ~handle =
    Map.find t.table handle
    |> Option.value_map
         ~f:(fun x ->
           let table = Map.remove t.table handle in
           t.table <- table;
           Ok x)
         ~default:Errors.doesnt_exist

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
    Map.find t.table ticket_handle
    |> Option.value_map
         ~f:(fun ({ Ticket_repr.ticket; amount; owner } as repr) ->
           let%ok () = assert_available sender owner in
           let table = Map.remove t.table ticket_handle in
           let ticket_repr = { repr with owner = To_be_dropped } in
           let handle = incr t in
           let table = Map.set ~key:handle ~data:ticket_repr table in
           t.table <- table;
           let to_return = (ticket, amount, handle) in
           Ok to_return)
         ~default:Errors.doesnt_exist

  let split_ticket t ~sender ~ticket_handle ~amounts =
    let%ok { Ticket_repr.ticket; amount; owner } =
      unsafe_read t ~handle:ticket_handle
    in
    let%ok () = assert_available sender owner in
    let%ok repr, repr2 =
      Ticket_repr.split ticket ~ticket_total:amount ~amounts
    in
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

  let init :
      self:Address.t ->
      mapping:((Ticket_id.t * Amount.t) * Ticket_handle.t) list ->
      tickets:(Ticket_id.t * Amount.t) Seq.t ->
      temporary_tickets:
        ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) Seq.t ->
      t * (Int64.t option * Ticket_handle.t) list option =
   fun ~self ~mapping ~tickets ~temporary_tickets ->
    let tickets = Stdlib.List.of_seq tickets in
    let mapping =
      List.filter
        ~f:(fun (arg, _) -> List.mem tickets arg ~equal:Poly.equal)
        mapping
    in
    let mmap = Set.of_list (module Int32) (List.map ~f:snd mapping) in
    let rec folder t =
      if Set.mem mmap t then folder Int32.(t + one) else Int32.(t + one)
    in
    let acc =
      {
        counter = Seq.unfold (fun acc -> Some (acc, folder acc)) Int32.min_value;
        table = Map.empty;
      }
    in
    let tickets_to_remap =
      Seq.fold_left
        (fun lst ((ticket, amount), (handle, offs)) ->
          let repr = Ticket_repr.make ticket amount Owner.To_be_dropped in
          let handle, offs =
            if Set.mem mmap handle then
              let new_handle = incr acc in
              (new_handle, Some (offs, new_handle))
            else (handle, None)
          in
          let table = Map.set ~key:handle ~data:repr acc.table in
          acc.table <- table;
          offs :: lst)
        [] temporary_tickets
    in
    let tickets_to_remap =
      List.fold_right
        ~f:(fun x acc ->
          match (x, acc) with
          | _, None -> None
          | None, Some _ -> None
          | Some x, Some acc -> Some (x :: acc))
        ~init:(Some []) tickets_to_remap
    in
    let () =
      List.iter
        ~f:(fun ((ticket, amount), handle) ->
          let data = Ticket_repr.make ticket amount (Owner.Owned self) in
          let table = Map.set ~key:handle ~data acc.table in
          acc.table <- table)
        mapping
    in
    (acc, tickets_to_remap)

  let finalize t =
    Map.fold
      ~f:(fun ~key ~data acc ->
        match data with
        | { owner = Owned _; ticket; amount } ->
            Seq.cons (key, ticket, amount) acc
        | { owner = To_be_dropped; ticket = _; amount = _ } -> acc)
      t.table ~init:Seq.empty
end
