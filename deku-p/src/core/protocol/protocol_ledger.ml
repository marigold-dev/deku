open Deku_concepts

(* TODO: probably monad using effects is a better idea to avoid allocations *)

type ledger_entry =
  | Ledger_entry of {
      owner : Contract.t;
      ticket : Ticket_id.t;
      balance : Amount.t;
    }

let ledger_entry_encoding =
  let open Data_encoding in
  conv
    (fun (Ledger_entry { owner; ticket; balance }) -> (owner, ticket, balance))
    (fun (owner, ticket, balance) -> Ledger_entry { owner; ticket; balance })
    (tup3 Contract.encoding Ticket_id.encoding Amount.encoding)

type ledger = ledger_entry Ledger_code.Map.t
type t = ledger

exception Ledger_limit
exception Invalid_code
exception Balance_not_enough
exception Owner_mismatch
exception Ticket_mismatch

(* repr *)
let encoding =
  let open Data_encoding in
  let rec of_list ledger list =
    match list with
    | [] -> ledger
    | (code, entry) :: tail ->
        (* TODO: n log n *)
        (* TODO: guarantee ordering of codes *)
        let ledger = Ledger_code.Map.add code entry ledger in
        of_list ledger tail
  in
  let of_list list = of_list Ledger_code.Map.empty list in
  let to_list ledger = Ledger_code.Map.bindings ledger in
  conv to_list of_list (list (tup2 Ledger_code.encoding ledger_entry_encoding))

let[@inline always] burn ~owner ~source amount ledger =
  let (Ledger_entry { owner = source_owner; ticket; balance }) =
    Ledger_code.Map.find source ledger
  in
  (match Contract.equal owner source_owner with
  | true -> ()
  | false -> raise Owner_mismatch);
  let balance =
    match Amount.(balance - amount) with
    | Some balance -> balance
    | None -> raise Balance_not_enough
  in
  let ledger =
    Ledger_code.Map.add source (Ledger_entry { owner; ticket; balance }) ledger
  in
  (ledger, ticket)

let[@inline always] deposit ~ticket ~destination amount ledger =
  let (Ledger_entry { owner; ticket = destination_ticket; balance }) =
    Ledger_code.Map.find destination ledger
  in
  (match Ticket_id.equal ticket destination_ticket with
  | true -> raise Ticket_mismatch
  | false -> ());
  let balance = Amount.(balance + amount) in
  Ledger_code.Map.add destination
    (Ledger_entry { owner; ticket; balance })
    ledger

let[@inline always] transfer ~owner ~source ~destination amount ledger =
  (* read order matters *)
  let ledger, ticket = burn ~owner ~source amount ledger in
  deposit ~ticket ~destination amount ledger

(* TODO: bench this guard performance *)
let[@inline always] guard_not_found f =
  try f () with Not_found -> raise Invalid_code

(* external *)
let initial = Ledger_code.Map.empty

let[@inline always] owner code ledger =
  guard_not_found @@ fun () ->
  let (Ledger_entry { owner; ticket = _; balance = _ }) =
    Ledger_code.Map.find code ledger
  in
  owner

let[@inline always] ticket code ledger =
  guard_not_found @@ fun () ->
  let (Ledger_entry { owner = _; ticket; balance = _ }) =
    Ledger_code.Map.find code ledger
  in
  ticket

let[@inline always] balance code ledger =
  guard_not_found @@ fun () ->
  let (Ledger_entry { owner = _; ticket = _; balance }) =
    Ledger_code.Map.find code ledger
  in
  balance

let[@inline always] burn ~owner ~source amount ledger =
  guard_not_found @@ fun () ->
  let ledger, _ticket = burn ~owner ~source amount ledger in
  ledger

let[@inline always] deposit ~ticket ~destination amount ledger =
  guard_not_found @@ fun () -> deposit ~ticket ~destination amount ledger

let[@inline always] transfer ~owner ~source ~destination amount ledger =
  guard_not_found @@ fun () ->
  transfer ~owner ~source ~destination amount ledger

let[@inline always] register ~owner ~ticket ledger =
  let code =
    match Ledger_code.Map.max_binding_opt ledger with
    | Some (max_code, _entry) -> (
        match Ledger_code.next max_code with
        | Some code -> code
        | None -> raise Ledger_limit)
    | None -> Ledger_code.zero
  in
  let balance = Amount.zero in
  let ledger =
    Ledger_code.Map.add code (Ledger_entry { owner; ticket; balance }) ledger
  in
  (ledger, code)
