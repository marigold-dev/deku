open Helpers

module type CTX = sig
  module Address : sig
    type t
    val size : int
    val of_bytes : bytes -> t
    val to_bytes : t -> bytes
  end
  module Ticket_handle : sig
    type t
    val size : int

    val of_bytes : bytes -> t
    val to_bytes : t -> bytes
  end
  module Ticket_id : sig
    type t
    val size : t -> int

    val to_bytes : t -> bytes
  end
  module Amount : sig
    type t
    val size : int
    val of_int : int -> t
    val to_int : t -> int
  end

  val get_source : Address.t
  val sender : unit -> Address.t
  val source : unit -> Address.t
  val self : unit -> Address.t
  val read_ticket : Ticket_handle.t -> Ticket_id.t * Amount.t * Ticket_handle.t
  val split_ticket :
    Ticket_handle.t * Amount.t * Amount.t -> Ticket_handle.t * Ticket_handle.t

  val join_tickets : Ticket_handle.t * Ticket_handle.t -> Ticket_handle.t
  val own_ticket : Ticket_handle.t -> Ticket_handle.t
  val get_contract_opt : Address.t -> Address.t option
  val transaction : bytes * (Ticket_handle.t * Amount.t) * Address.t -> int
end

module Make (C : CTX) : sig
  val custom : Memory.t -> int64 -> unit
end = struct
  type syscall =
    | SENDER
    | SOURCE
    | SELF
    | OWN_TICKET
    | READ_TICKET
    | JOIN_TICKETS
    | SPLIT_TICKET
    | GET_CONTRACT_OPT
    | TRANSACTION
  open struct
    open C
    let int32_offset = 5
    let int64_offset = 9
    module W = struct
      let write_address ~writer_addr addr memory =
        let bytes = Address.to_bytes addr in
        Memory.store_bytes memory ~address:writer_addr ~content:bytes
      let write_ticket ~writer_addr ticket memory =
        let content = Ticket_handle.to_bytes ticket in
        Memory.store_bytes memory ~address:writer_addr ~content
      let write_tickets ~writer_addr tickets memory =
        let content =
          List.map C.Ticket_handle.to_bytes tickets |> Bytes.concat Bytes.empty
        in
        Memory.store_bytes memory ~address:writer_addr ~content
      let write_some ~writer_addr item writer memory =
        let i32 t =
          let b = Bytes.make 4 '0' in
          let () = Bytes.set_int32_le b 0 (Int32.of_int t) in
          b in
        match item with
        | None ->
          Memory.store_bytes ~address:writer_addr memory ~content:(i32 0)
        | Some item ->
          Memory.store_bytes ~address:writer_addr memory ~content:(i32 1);
          writer
            ~writer_addr:(Int64.add writer_addr (Int64.of_int int32_offset))
            item memory
    end
    module R = struct
      let read_ticket memory addr offs =
        let new_addr = Int64.add addr (offs |> Int64.of_int) in
        let addr =
          Bytes.get_int32_le
            (Memory.load_bytes memory ~address:new_addr ~size:4)
            0
          |> Int64.of_int32 in
        let ticket_handle =
          Memory.load_bytes memory ~address:addr ~size:Ticket_handle.size in
        let ticket_handle = Ticket_handle.of_bytes ticket_handle in
        (new_addr, ticket_handle)
      let read_amount memory addr offs =
        let new_addr = Int64.add addr (offs |> Int64.of_int) in
        let res =
          Bytes.get_int64_le
            (Memory.load_bytes memory ~address:new_addr ~size:Amount.size)
            0
          |> Int64.to_int
          |> Amount.of_int in
        (new_addr, res)
      let read_address memory addr offs =
        let new_addr = Int64.add addr (offs |> Int64.of_int) in
        let addr =
          Bytes.get_int32_le
            (Memory.load_bytes ~address:new_addr ~size:4 memory)
            0 in
        let res =
          Memory.load_bytes ~address:(Int64.of_int32 addr) ~size:Address.size
            memory
          |> Address.of_bytes in
        (new_addr, res)
    end
  end
  let load memory writer_addr syscall =
    match syscall with
    | SENDER -> W.write_address ~writer_addr (C.sender ()) memory
    | SOURCE -> W.write_address ~writer_addr (C.source ()) memory
    | SELF -> W.write_address ~writer_addr (C.self ()) memory
    | READ_TICKET ->
      let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
      let store (ticket, amount, ticket_handle) =
        let buf = Bytes.make 8 '\x00' in
        let amount = C.Amount.to_int amount |> Int64.of_int in
        Bytes.set_int64_le buf 0 amount;
        let ticket = C.Ticket_id.to_bytes ticket in
        let ticket_handle = C.Ticket_handle.to_bytes ticket_handle in
        let content = Bytes.concat Bytes.empty [ticket; buf; ticket_handle] in
        Memory.store_bytes memory ~address:writer_addr ~content in
      store (C.read_ticket ticket_handle)
    | SPLIT_TICKET ->
      let addr, ticket_handle = R.read_ticket memory writer_addr int32_offset in
      let addr, amount1 = R.read_amount memory addr int32_offset in
      let _, amount2 = R.read_amount memory addr int64_offset in
      let data = (ticket_handle, amount1, amount2) in
      let store (handle1, handle2) =
        W.write_tickets ~writer_addr [handle1; handle2] memory in
      store (C.split_ticket data)
    | OWN_TICKET ->
      let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
      let store handle1 = W.write_ticket ~writer_addr handle1 memory in
      store (C.own_ticket ticket_handle)
    | JOIN_TICKETS ->
      let addr, ticket_handle = R.read_ticket memory writer_addr int32_offset in
      let _, ticket_handle2 = R.read_ticket memory addr int32_offset in
      let store handle1 = W.write_ticket ~writer_addr handle1 memory in
      store (C.join_tickets (ticket_handle, ticket_handle2))
    | TRANSACTION ->
      let addr =
        Bytes.get_int32_le
          (Memory.load_bytes memory
             ~address:(Int64.add writer_addr (int32_offset |> Int64.of_int))
             ~size:4)
          0
        |> Int64.of_int32 in
      let arg =
        if addr = -1L then
          Bytes.empty
        else
          let size =
            Bytes.get_int64_le
              (Memory.load_bytes memory ~address:addr ~size:8)
              0
            |> Int64.to_int in
          let bytes =
            Memory.load_bytes memory ~address:(Int64.add addr 8L) ~size in
          bytes in
      let addr, ticket_handle =
        R.read_ticket memory writer_addr (int32_offset * 2) in
      let addr, amount = R.read_amount memory addr int32_offset in
      let _, destination = R.read_address memory addr int64_offset in
      let store int =
        let buf = Bytes.make 4 '\x00' in
        Bytes.set_int32_le buf 0 (Int32.of_int int);
        Memory.store_bytes memory ~address:writer_addr ~content:buf in
      let called = C.transaction (arg, (ticket_handle, amount), destination) in
      store called
    | GET_CONTRACT_OPT ->
      let _, address = R.read_address memory writer_addr int32_offset in
      let store addr = W.write_some ~writer_addr addr W.write_address memory in
      store (C.get_contract_opt address)
  let custom mem x =
    match
      Bytes.get_int32_le (Memory.load_bytes mem ~address:x ~size:4) 0
      |> Int32.to_int
    with
    | 0 -> load mem x SELF
    | 1 -> load mem x SOURCE
    | 2 -> load mem x SENDER
    | 3 -> load mem x READ_TICKET
    | 4 -> load mem x SPLIT_TICKET
    | 5 -> load mem x OWN_TICKET
    | 6 -> load mem x JOIN_TICKETS
    | 7 -> load mem x GET_CONTRACT_OPT
    | 8 -> load mem x TRANSACTION
    | _ -> failwith "unimplemented"
end
