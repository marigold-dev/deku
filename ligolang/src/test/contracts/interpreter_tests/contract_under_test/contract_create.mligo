type storage = address option
type return = operation list * storage
type parameter = One | Two

// let main_fail (action, store : parameter * storage) : return =
//   (failwith "main fail !" : return)

let main (action, store : parameter * storage) : return =
  match action with
    | One ->
      let a = match store with
        | None -> (failwith 1 : address)
        | Some a -> a
      in
      let c : nat contract option = Tezos.get_contract_opt a in
      let ops = match c with
          Some (c) -> [ Tezos.transaction 1n 10tez c ]
        | None     -> (failwith 2 : operation list)
      in
      (ops, (None: storage))
    | Two ->
      let x : operation * address = Tezos.create_contract
        (fun (p, s : nat * string) -> (failwith 111: operation list * string))
        (None: key_hash option) 
        1tz
        "un"
      in
      ([x.0], (Some x.1))

type rect = { toto : int ; titi : int}
