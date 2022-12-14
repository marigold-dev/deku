open Deku_crypto
open Deku_concepts

(* TODO: probably monad using effects is a better idea to avoid allocations *)
exception Ledger_limit
exception Invalid_owner
exception Invalid_code
exception Ticket_mismatch
exception Insufficient_funds

module Burn_receipt_tree = Incremental_patricia.Make (struct
  include Burn_receipt

  let hash burn_receipt =
    let open Burn_receipt in
    let (Burn_receipt { hash; _ }) = burn_receipt in
    Burn_receipt_hash.to_blake2b hash
end)

(* module Array : sig
     module Immutable : sig
       type 'a immutable_array
       type 'a t = 'a array

       val get : 'a immutable_array -> int -> 'a
     end

     module Mutable : sig
       type 'a mutable_array
       type 'a t = 'a mutable_array

       val make : int -> 'a -> 'a array

       (* safe API *)
       val get : 'a mutable_array -> int -> 'a
       val set : 'a mutable_array -> int -> 'a -> unit

       (* maybe enforce linearity? *)
       val to_immutable : 'a mutable_array -> 'a Immutable.t
       val of_immutable : 'a Immutable.t -> 'a mutable_array
     end
   end = struct
     module Immutable = struct
       include Array

       type 'a immutable_array = 'a t
     end

     module Mutable = struct
       include Array

       type 'a mutable_array = 'a t

       let to_immutable x = x
       let of_immutable x = x
     end
   end *)

let deku_contract_hash_placeholder =
  let weird_place_holder = BLAKE2b.hash "weird place holder" in
  Deku_contract_address.of_user_operation_hash weird_place_holder

let contract_placeholder =
  Contract.of_deku_contract_address deku_contract_hash_placeholder

let ticket_placeholder =
  let ticketer = deku_contract_hash_placeholder in
  let data = Bytes.create 0 in
  Ticket_id.mint_deku_ticket ~ticketer ~data

(* TODO: cell to a different module? *)
type cell = {
  owner : Contract.t;
  ticket : Ticket_id.t;
  mutable amount : Amount.t;
}

let local_default = false
let shared_default = true

module Writer = struct
  (* TODO: more information*)
  exception Not_enough
  exception Invalid_code

  type writer = {
    (* TODO: make it at least bytes, otherwise, incredibly inneficient *)
    mutable entry_is_local : bool array;
    mutable entry_is_shared : bool array;
    mutable entry_writer : cell array;
    mutable entry_shared : cell array;
    mutable next_code : Ledger_code.t;
    mutable receipts : Burn_receipt_tree.t;
  }

  let cell_placeholder =
    {
      owner = contract_placeholder;
      ticket = ticket_placeholder;
      amount = Amount.zero;
    }

  let double source initial =
    let source_length = Array.length source in
    let destination_length = max (source_length * 2) 1 in
    let destination = Array.make destination_length initial in
    Array.blit source 0 destination 0 source_length;
    destination

  let double ledger =
    (* TODO: O(n) may be bad *)
    ledger.entry_is_local <- double ledger.entry_is_local local_default;
    ledger.entry_is_shared <- double ledger.entry_is_shared shared_default;
    ledger.entry_local <- double ledger.entry_local cell_placeholder;
    ledger.entry_shared <- double ledger.entry_shared cell_placeholder

  let get ledger code =
    let code = Ledger_code.to_int code in
    match Array.get ledger.entry_is_local code with
    | true -> Array.get ledger.entry_local code
    | false ->
        (* TODO: allocation? *)
        let { owner; ticket; amount } = Array.get ledger.entry_shared code in
        { owner; ticket; amount }

  let set ledger code cell =
    let code = Ledger_code.to_int code in
    Array.set ledger.entry_is_local code true;
    Array.set ledger.entry_is_shared code false;
    Array.set ledger.entry_local code cell

  let burn ledger ~source amount =
    let cell = get ledger source in
    (match Amount.(cell.amount - amount) with
    | Some amount -> cell.amount <- amount
    | None -> raise Not_enough);
    set ledger source cell

  let deposit ledger ~destination amount =
    let cell = get ledger destination in
    cell.amount <- Amount.(cell.amount + amount);
    set ledger destination cell

  let reserve_code ledger =
    let code = ledger.next_code in
    let next_code =
      match Ledger_code.next code with
      | Some next_code -> next_code
      | None -> raise Ledger_limit
    in
    ledger.next_code <- next_code;
    (match
       (* TODO: this is not ideal *)
       let size = Array.length ledger.entry_is_local in
       let code = Ledger_code.to_int code in
       code < size
     with
    | true -> ()
    | false -> double ledger);
    code

  let register ledger ~owner ~ticket =
    let code = reserve_code ledger in
    let () = set ledger code { owner; ticket; amount = Amount.zero } in
    code

  (* checks *)
  let check_code ledger code =
    match Ledger_code.(code < ledger.next_code) with
    | true -> ()
    | false -> raise Invalid_code

  let check_owner ledger ~owner code =
    let found = get ledger code in
    match Contract.equal owner found.owner with
    | true -> ()
    | false -> raise Invalid_owner

  let check_ticket_match ledger ~source ~destination =
    let source = get ledger source in
    let destination = get ledger destination in
    match Ticket_id.equal source.ticket destination.ticket with
    | true -> ()
    | false -> raise Ticket_mismatch

  let check_deposit_ticket_match ledger ~expected_ticket ~destination =
    let destination = get ledger destination in
    match Ticket_id.equal expected_ticket destination.ticket with
    | true -> ()
    | false -> raise Ticket_mismatch

  let guard_invalid_argument f =
    try f () with Invalid_argument _ -> raise Invalid_code
  (* from Array.{get, set} *)

  (* external *)
  let transfer ledger ~owner ~source ~destination amount =
    guard_invalid_argument @@ fun () ->
    check_code ledger source;
    check_code ledger destination;
    check_owner ledger ~owner source;
    check_ticket_match ledger ~source ~destination;
    burn ledger ~source amount;
    deposit ledger ~destination amount

  let deposit ledger ~expected_ticket ~destination amount =
    guard_invalid_argument @@ fun () ->
    check_code ledger destination;
    check_deposit_ticket_match ledger ~expected_ticket ~destination;
    deposit ledger ~destination amount

  let withdraw ledger ~deku_ticketer ~owner ~source ~destination amount =
    guard_invalid_argument @@ fun () ->
    check_code ledger source;
    check_owner ledger ~owner source;
    burn ledger ~source amount;
    let receipts, receipt =
      Burn_receipt_tree.add
        (fun id ->
          let ticket_id =
            let source = get ledger source in
            Ticket_id.encode_tezos_ticket_id ~deku_ticketer source.ticket
          in
          (* TODO: owner is a bad name,
              this is the guy that can withdraw on destination *)
          Burn_receipt.make ~id ~owner:destination ~ticket_id ~amount)
        ledger.receipts
    in
    ledger.receipts <- receipts;
    receipt
end

module Reader = struct
  type reader = {
    entry_is_shared : bool array;
    entry_reader : cell array;
    entry_shared : cell array;
    next_code : Ledger_code.t;
    receipts : Burn_receipt_tree.t;
  }

  let commit ledger =
    let { entry_is_shared; entry_local; entry_shared; next_code; receipts = _ }
        =
      ledger
    in
    let length = Ledger_code.to_int next_code in
    let rec copy_and_clear_loop code =
      match code < length with
      | true ->
          (match Array.get entry_is_shared code with
          | true ->
              let cell = Array.get entry_shared code in
              Array.set entry_local code cell
          | false -> ());
          Array.set entry_is_shared code shared_default;
          copy_and_clear_loop (code + 1)
      | false -> ()
    in
    copy_and_clear_loop 0;
    ()
end

let switch reader writer =
  let open Reader in
  let open Writer in
  let entry_is_default = assert false in
  let entry_is_shared = assert false in
  let entry_is_local = assert false in
  let entry_writer = assert false in
  let entry_shared = assert false in
  let entry_reader = assert false in
  let next_code = assert false in
  let receipts = assert false in
  let reader =
    {
      entry_is_shared = entry_is_local;
      entry_reader = entry_shared;
      entry_shared = entry_writer;
      next_code;
      receipts;
    }
  in
  let writer =
    {
      entry_is_local = entry_is_shared;
      entry_is_shared = entry_is_default;
      entry_writer = entry_reader;
      entry_shared = entry_writer;
      next_code;
      receipts;
    }
  in
  assert false

(* TODO: Vector.t and tested *)
type ledger = {
  mutable owners : Contract.t array;
  mutable tickets : Ticket_id.t array;
  mutable balances : Amount.t array;
  mutable receipts : Burn_receipt_tree.t;
  mutable next_code : Ledger_code.t;
}

type t = ledger

(* TODO: better place holders or maybe option? *)

let create ~size =
  let owners = Array.make size contract_placeholder in
  let tickets = Array.make size ticket_placeholder in
  let balances = Array.make size Amount.zero in
  let receipts = Burn_receipt_tree.empty in
  let next_code = Ledger_code.zero in
  { owners; tickets; balances; receipts; next_code }

let double source initial =
  let source_length = Array.length source in
  let destination_length = source_length * 2 in
  let destination = Array.make destination_length initial in
  Array.blit source 0 destination 0 source_length;
  destination

let double ledger =
  (* TODO: O(n) may be bad *)
  ledger.owners <- double ledger.owners contract_placeholder;
  ledger.tickets <- double ledger.tickets ticket_placeholder;
  ledger.balances <- double ledger.balances Amount.zero

(* internal *)
let read_owner ledger code =
  match
    let code = Ledger_code.to_int code in
    Array.get ledger.owners code
  with
  | balance -> balance
  | exception Invalid_argument _ -> raise Invalid_code

let read_balance ledger code =
  match
    let code = Ledger_code.to_int code in
    Array.get ledger.balances code
  with
  | balance -> balance
  | exception Invalid_argument _ -> raise Invalid_code

let store_balance ledger code balance =
  match
    let code = Ledger_code.to_int code in
    Array.set ledger.balances code balance
  with
  | balance -> balance
  | exception Invalid_argument _ -> raise Invalid_code

let read_ticket ledger code =
  match
    let code = Ledger_code.to_int code in
    Array.get ledger.tickets code
  with
  | balance -> balance
  | exception Invalid_argument _ -> raise Invalid_code

let burn ledger ~source amount =
  let balance = read_balance ledger source in
  match Amount.(balance - amount) with
  | Some balance -> store_balance ledger source balance
  | None -> raise Insufficient_funds

let deposit ledger ~destination amount =
  let balance = read_balance ledger destination in
  let balance = Amount.(balance + amount) in
  store_balance ledger destination balance

(* checks *)
let check_code ledger code =
  match Ledger_code.(code < ledger.next_code) with
  | true -> ()
  | false -> raise Invalid_code

let check_owner ledger ~owner code =
  let found = read_owner ledger code in
  match Contract.equal owner found with
  | true -> ()
  | false -> raise Invalid_owner

let check_ticket_match ledger ~source ~destination =
  let source_ticket = read_ticket ledger source in
  let destination_ticket = read_ticket ledger destination in
  match Ticket_id.equal source_ticket destination_ticket with
  | true -> ()
  | false -> raise Ticket_mismatch

let check_deposit_ticket_match ledger ~expected_ticket ~destination =
  let destination_ticket = read_ticket ledger destination in
  match Ticket_id.equal expected_ticket destination_ticket with
  | true -> ()
  | false -> raise Ticket_mismatch

(* external *)
let owner ledger code =
  check_code ledger code;
  read_owner ledger code

let balance ledger code =
  check_code ledger code;
  read_balance ledger code

let transfer ledger ~owner ~source ~destination amount =
  check_code ledger source;
  check_code ledger destination;
  check_owner ledger ~owner source;
  check_ticket_match ledger ~source ~destination;
  burn ledger ~source amount;
  deposit ledger ~destination amount

let deposit ledger ~expected_ticket ~destination amount =
  check_code ledger destination;
  check_deposit_ticket_match ledger ~expected_ticket ~destination;
  deposit ledger ~destination amount

let withdraw ledger ~deku_ticketer ~owner ~source ~destination amount =
  check_code ledger source;
  check_owner ledger ~owner source;
  burn ledger ~source amount;
  let receipts, receipt =
    Burn_receipt_tree.add
      (fun id ->
        let ticket_id =
          let ticket_id = read_ticket ledger source in
          Ticket_id.encode_tezos_ticket_id ~deku_ticketer ticket_id
        in
        (* TODO: owner is a bad name,
            this is the guy that can withdraw on destination *)
        Burn_receipt.make ~id ~owner:destination ~ticket_id ~amount)
      ledger.receipts
  in
  ledger.receipts <- receipts;
  receipt

let reserve_code ledger =
  let code = ledger.next_code in
  match Ledger_code.next code with
  | Some next_code ->
      ledger.next_code <- next_code;
      (match
         let size = Array.length ledger.owners in
         let code = Ledger_code.to_int code in
         code < size
       with
      | true -> ()
      | false -> double ledger);
      code
  | None -> raise Ledger_limit

let register ledger ~owner ~ticket =
  let code = reserve_code ledger in
  let () =
    let code = Ledger_code.to_int code in
    Array.set ledger.owners code owner;
    Array.set ledger.tickets code ticket;
    Array.set ledger.balances code Amount.zero
  in
  code

let find_burn_receipt ~id ledger =
  match Burn_receipt_tree.find id ledger.receipts with
  | Some (proof, receipt) -> Some (receipt, proof)
  | None -> None

let burn_receipt_root ledger = Burn_receipt_tree.hash ledger.receipts

let encoding =
  let open Data_encoding in
  conv
    (fun { owners; tickets; balances; receipts; next_code } ->
      (owners, tickets, balances, receipts, next_code))
    (fun (owners, tickets, balances, receipts, next_code) ->
      { owners; tickets; balances; receipts; next_code })
    (tup5 (array Contract.encoding) (array Ticket_id.encoding)
       (array Amount.encoding) Burn_receipt_tree.encoding Ledger_code.encoding)
