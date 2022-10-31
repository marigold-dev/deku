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

let to_json_api (t : t) =
  match t with
  | State state ->
      let acc =
        Table.fold
          (fun contract_address state_entry acc ->
            let string = Contract_address.to_b58 contract_address in
            let json = State_entry.yojson_of_t state_entry in
            (string, json) :: acc)
          state []
      in
      `Assoc acc

let yojson_of_t t =
  `String
    (Data_encoding.Json.construct encoding t |> Data_encoding.Json.to_string)

let t_of_yojson t =
  match t with
  | `String string -> (
      try
        Data_encoding.Json.from_string string
        |> Result.map (fun x -> Data_encoding.Json.destruct encoding x)
        |> Result.get_ok
      with Invalid_argument _ -> raise Not_a_state)
  | _ -> raise Not_a_state
