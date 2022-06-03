open Core

module Contract_operation = struct
  type t =
    | Transfer of {
        destination : Address.t;
        ticket : Ticket_id.t;
        amount : Amount.t;
      }
    | Invoke   of {
        param : bytes;
        destination : Address.t;
        ticket : Ticket_id.t;
      }
end
module type CTX = sig
  module Address : sig
    type t = Address.t
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

  val get_table : unit -> Ticket_table.t
  val get_ops :
    int list -> (Contract_operation.t list, [`Execution_error]) result

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
module S = Helpers.Set.Make (Ticket_handle)
let get_or_raise t =
  Result.map_error
    ~f:(fun e ->
      Format.asprintf "%a" Ticket_table.Errors.pp (e :> Ticket_table.Errors.t))
    t
  |> Result.ok_or_failwith

let make ~sender ~table ~self ~source ~tickets ~contracts_table =
  (module struct
    module Address = struct
      include Address
      let size = 36
      let to_bytes t = Bytes.of_string (to_string t)
      let of_bytes t =
        let addr = Address.of_string (Bytes.to_string t) in
        Option.value_exn addr
    end
    module Ticket_handle = struct
      include Ticket_handle
      let size = 20
      let of_bytes t = Option.value_exn (of_bytes t)
    end
    module Ticket_id = struct
      include Ticket_id
      let size t = 36 + Bytes.length t.data
      let to_bytes t =
        let byt = Tezos.Address.to_string t.ticketer in
        Bytes.of_string byt
    end
    module Amount = struct
      include Amount
      let size = 36
      let of_int t = Amount.of_int t
      let to_int t = Amount.to_int t
    end
    let get_source = source

    let operations = ref []

    let get_ops received =
      let deduped = Set.stable_dedup_list (module Int) received in
      if List.equal Int.equal received deduped then
        let operations = !operations in
        let ops =
          List.fold_left
            ~f:(fun acc x ->
              let elem = List.Assoc.find_exn ~equal:Int.equal operations x in
              elem :: acc)
            ~init:[] received in
        Result.return ops
      else
        Result.fail `Execution_error

    let contracts_table address = contracts_table address

    let table = ref table

    let get_table () = !table

    let tickets =
      let t = Ticket_table.add_to_temporary !table tickets in
      let tickets = S.of_list tickets in
      table := t;
      ref tickets

    let sender = sender
    let self = Address.of_contract_hash self
    let source = source

    let correct_addr ticket_handle =
      if S.mem ticket_handle !tickets then
        source
      else
        self

    let read_ticket ticket_handle =
      let sender = correct_addr ticket_handle in
      let (ticket, amount, handle), t =
        Ticket_table.read_ticket ~sender ~ticket_handle !table |> get_or_raise
      in
      table := t;
      (ticket, amount, handle)

    let own_ticket ticket_handle =
      let ticket_handle, t =
        Ticket_table.own !table self ticket_handle |> get_or_raise in
      table := t;
      ticket_handle

    let split_ticket (handle, amount1, amount2) =
      let sender = correct_addr handle in
      let (handle1, handle2), t =
        Ticket_table.split_ticket ~ticket_handle:handle ~sender
          ~amounts:(amount1, amount2) !table
        |> get_or_raise in
      table := t;
      (handle1, handle2)

    let get_contract_opt address =
      if Address.is_implicit address then
        Some address
      else
        let contract_address =
          Address.to_contract_hash address |> Option.value_exn in
        contracts_table contract_address |> Option.map ~f:(Fn.const address)

    let join_tickets ((handle1, handle2) as handles) =
      let sender1 = correct_addr handle1 in
      let sender2 = correct_addr handle2 in
      let handle, t =
        Ticket_table.join_tickets ~source:self ~senders:(sender1, sender2)
          ~handles !table
        |> get_or_raise in
      table := t;
      handle

    let transaction (param, (ticket_handle, amount), address) =
      let sender = correct_addr ticket_handle in
      let (ticket, amount2, handle), t =
        Ticket_table.read_ticket ~sender ~ticket_handle !table |> get_or_raise
      in
      if not (Amount.equal amount amount2) then failwith "execution error";
      table := t;
      let handle, t = Ticket_table.own !table self handle |> get_or_raise in
      table := t;
      tickets := S.remove handle !tickets;
      let ops = !operations in
      let operation =
        if Address.is_implicit address then
          Contract_operation.Transfer { ticket; amount; destination = address }
        else
          let param =
            String.substr_replace_all (Bytes.to_string param)
              ~pattern:(Ticket_handle.to_bytes ticket_handle |> Bytes.to_string)
              ~with_:(Ticket_handle.to_bytes handle |> Bytes.to_string)
            |> Bytes.of_string in
          Contract_operation.Invoke { param; ticket; destination = address }
      in
      let idx, ops =
        match ops with
        | [] -> (1, [(1, operation)])
        | ((num, _) as x) :: xs -> (num + 1, (num + 1, operation) :: x :: xs)
      in

      operations := ops;
      idx

    let sender () = sender
    let self () = self
    let source () = source
  end : CTX)
