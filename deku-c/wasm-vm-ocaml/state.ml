open Deku_ledger
module Table = Map.Make (Contract_address)

exception Not_a_state

type t = State : State_entry.t Table.t -> t [@@unboxed] [@@deriving ord]

let show (State t) =
  [%show: (Contract_address.t * State_entry.t) list] (Table.bindings t)

let empty = State Table.empty

let pp fmt (State t) =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@")
       (fun fmt (k, v) ->
         Format.fprintf fmt "%a -> %a" Contract_address.pp k State_entry.pp v))
    (Table.bindings t)

let add_contract (State t) address entry = State (Table.add address entry t)
let fetch_contract (State t) address = Table.find address t

let encoding =
  let open Data_encoding in
  conv
    (function State x -> Table.bindings x)
    (function lst -> State (Table.of_seq @@ List.to_seq lst))
    (list
       (tup2
          (dynamic_size Contract_address.encoding)
          (dynamic_size State_entry.encoding)))

let api_encoding =
  Data_encoding.conv
    (fun state ->
      let (State state) = state in
      Table.fold
        (fun key value acc ->
          let key = Contract_address.to_b58 key in
          (key, value) :: acc)
        state [])
    (fun list ->
      List.fold_left
        (fun acc (key, value) ->
          let address = Contract_address.of_b58 key |> Option.get in
          add_contract acc address value)
        empty list)
    (Data_encoding.assoc State_entry.encoding)
