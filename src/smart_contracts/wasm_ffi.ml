open Stdlib
open Conversions
open Contract_ffi

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
  | MINT_TICKET

open struct
  open Wasm_vm

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
        List.map Ticket_handle.to_bytes tickets |> Bytes.concat Bytes.empty
      in
      Memory.store_bytes memory ~address:writer_addr ~content

    let write_some ~writer_addr item writer memory =
      let i32 t =
        let b = Bytes.make 4 '0' in
        let () = Bytes.set_int32_le b 0 (Int32.of_int t) in
        b in
      match item with
      | None -> Memory.store_bytes ~address:writer_addr memory ~content:(i32 0)
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

    let read_ticket_with_addr memory addr offs =
      let new_addr = Int64.add addr (offs |> Int64.of_int) in
      let addr =
        Bytes.get_int32_le
          (Memory.load_bytes memory ~address:new_addr ~size:4)
          0
        |> Int64.of_int32 in
      let ticket_handle =
        Memory.load_bytes memory ~address:addr ~size:Ticket_handle.size in
      let ticket_handle = Ticket_handle.of_bytes ticket_handle in
      (new_addr, ticket_handle, addr)

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

open Wasm_vm

let load ~ctx memory writer_addr syscall =
  match syscall with
  | SENDER -> W.write_address ~writer_addr (Addressing.sender ctx) memory
  | SOURCE -> W.write_address ~writer_addr (Addressing.source ctx) memory
  | SELF -> W.write_address ~writer_addr (Addressing.self ctx) memory
  | READ_TICKET ->
    let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
    let store (ticket, amount, ticket_handle) =
      let buf = Bytes.make 8 '\x00' in
      let amount = Amount.to_int amount |> Int64.of_int in
      Bytes.set_int64_le buf 0 amount;
      let ticket = Ticket_id.to_bytes ticket in
      let ticket_handle = Ticket_handle.to_bytes ticket_handle in
      let content = Bytes.concat Bytes.empty [ticket; buf; ticket_handle] in
      Memory.store_bytes memory ~address:writer_addr ~content in
    store (Table_ops.read_ticket ctx ticket_handle)
  | SPLIT_TICKET ->
    let addr, ticket_handle = R.read_ticket memory writer_addr int32_offset in
    let addr, amount1 = R.read_amount memory addr int32_offset in
    let _, amount2 = R.read_amount memory addr int64_offset in
    let data = (ticket_handle, amount1, amount2) in
    let store (handle1, handle2) =
      W.write_tickets ~writer_addr [handle1; handle2] memory in
    store (Table_ops.split_ticket ctx data)
  | OWN_TICKET ->
    let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
    let store handle1 = W.write_ticket ~writer_addr handle1 memory in
    store (Table_ops.own_ticket ctx ticket_handle)
  | JOIN_TICKETS ->
    let addr, ticket_handle = R.read_ticket memory writer_addr int32_offset in
    let _, ticket_handle2 = R.read_ticket memory addr int32_offset in
    let store handle1 = W.write_ticket ~writer_addr handle1 memory in
    store (Table_ops.join_tickets ctx (ticket_handle, ticket_handle2))
  | TRANSACTION ->
    let addr =
      Bytes.get_int32_le
        (Memory.load_bytes memory
           ~address:(Int64.add writer_addr (int32_offset |> Int64.of_int))
           ~size:4)
        0
      |> Int64.of_int32 in
    let arg, stat =
      if addr = -1L then
        (Bytes.empty, None)
      else
        let size =
          Bytes.get_int64_le (Memory.load_bytes memory ~address:addr ~size:8) 0
          |> Int64.to_int in
        let bytes =
          Memory.load_bytes memory ~address:(Int64.add addr 8L) ~size in
        (bytes, Some Int64.(add addr 8L)) in
    let addr, ticket_handle, address =
      R.read_ticket_with_addr memory writer_addr (int32_offset * 2) in
    let stat = stat |> Option.map (fun x -> Int64.sub address x) in
    let addr, amount = R.read_amount memory addr int32_offset in
    let _, destination = R.read_address memory addr int64_offset in
    let store int =
      let buf = Bytes.make 4 '\x00' in
      Bytes.set_int32_le buf 0 (Int32.of_int int);
      Memory.store_bytes memory ~address:writer_addr ~content:buf in
    let called =
      Operations.transaction ctx
        (arg, (ticket_handle, amount, stat), destination) in
    store called
  | GET_CONTRACT_OPT ->
    let _, address = R.read_address memory writer_addr int32_offset in
    let store addr = W.write_some ~writer_addr addr W.write_address memory in
    store (Addressing.get_contract_opt ctx address)
  | MINT_TICKET ->
    let addr =
      Bytes.get_int32_le
        (Memory.load_bytes memory
           ~address:(Int64.add writer_addr (int32_offset |> Int64.of_int))
           ~size:4)
        0
      |> Int64.of_int32 in
    let _, amount = R.read_amount memory addr 0 in
    let payload =
      let size =
        Bytes.get_int32_le
          (Memory.load_bytes memory
             ~address:(Int64.add addr (int64_offset |> Int64.of_int))
             ~size:4)
          0 in
      Memory.load_bytes memory
        ~address:(Int64.add addr (Int64.of_int (int64_offset + int32_offset)))
        ~size:(Int32.to_int size) in
    let ticket = Table_ops.mint_ticket ctx (payload, amount) in
    W.write_ticket ~writer_addr:addr ticket memory

let custom ~ctx mem x =
  match
    Bytes.get_int32_le (Memory.load_bytes mem ~address:x ~size:4) 0
    |> Int32.to_int
  with
  | 0 -> load ~ctx mem x SELF
  | 1 -> load ~ctx mem x SOURCE
  | 2 -> load ~ctx mem x SENDER
  | 3 -> load ~ctx mem x READ_TICKET
  | 4 -> load ~ctx mem x SPLIT_TICKET
  | 5 -> load ~ctx mem x OWN_TICKET
  | 6 -> load ~ctx mem x JOIN_TICKETS
  | 7 -> load ~ctx mem x GET_CONTRACT_OPT
  | 8 -> load ~ctx mem x TRANSACTION
  | 9 -> load ~ctx mem x MINT_TICKET
  | _ -> failwith "unimplemented"
