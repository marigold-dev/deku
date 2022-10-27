open Deku_stdlib
module Table = Map.Make (Deku_ledger.Ticket_id)

type t = N.t Table.t ref

let from_seq seq : t = ref @@ Table.of_seq seq

let take t id amount =
  let table = !t in
  let map =
    Table.update id
      (function
        | Some x -> (
            match N.(x - amount) with
            | Some x -> Some x
            | None -> raise (Ticket_table.Table `Ownership_violation))
        | None -> raise (Ticket_table.Table `Ownership_violation))
      table
  in
  t := map

let rec from_runtime_ticket (table : (Deku_ledger.Ticket_id.t * N.t) list)
    ticket_table (t : Value.t) =
  let open Value in
  match t with
  | (Int _ | String _ | Bytes _ | Bool _) as x -> (x, table)
  | Pair (x, y) ->
      let x, res = from_runtime_ticket table ticket_table x in
      let y, res = from_runtime_ticket res ticket_table y in
      (Pair (x, y), res)
  | Union (Left x) ->
      let x, res = from_runtime_ticket table ticket_table x in
      (Union (Left x), res)
  | Union (Right x) ->
      let x, res = from_runtime_ticket table ticket_table x in
      (Union (Right x), res)
  | List (x, tag) ->
      let x, res =
        List.fold_left
          (fun (l, acc) x ->
            let x, res = from_runtime_ticket acc ticket_table x in
            (x :: l, res))
          ([], table) x
      in

      (List (x, tag), res)
  | Option (Some x) ->
      let x, res = from_runtime_ticket table ticket_table x in
      (Option (Some x), res)
  | Option None -> (Option None, table)
  | Unit -> (Unit, table)
  | Map m ->
      let x, res =
        Map.fold
          (fun k v (s, acc) ->
            let k, res = from_runtime_ticket acc ticket_table k in
            let v, res = from_runtime_ticket res ticket_table v in
            (Seq.cons (k, v) s, res))
          m (Seq.empty, table)
      in
      (Map (Map.of_seq x), res)
  | Closure _ -> raise (Ticket_table.Table `Ticket_doesnt_exist)
  | Ticket _ -> raise (Ticket_table.Table `Ticket_doesnt_exist)
  | Ticket_handle x ->
      let ({ ticket_id; amount; alive = _ } : Ticket_table.Entry.t) =
        Ticket_table.unsafe_read ticket_table x
      in
      ( Ticket { ticket_id; amount = Deku_concepts.Amount.of_n amount },
        (ticket_id, amount) :: table )
  | Set l ->
      let x, res =
        Set.fold
          (fun k (l, table) ->
            let x, res = from_runtime_ticket table ticket_table k in
            (x :: l, res))
          l ([], table)
      in
      (Set (Set.of_list x), res)

let rec to_runtime_ticket temp ticket_table (t : Value.t) =
  let open Value in
  match t with
  | (Int _ | String _ | Bytes _ | Bool _) as x -> x
  | Pair (x, y) ->
      let x = to_runtime_ticket temp ticket_table x in
      let y = to_runtime_ticket temp ticket_table y in
      Pair (x, y)
  | Union (Left x) ->
      let x = to_runtime_ticket temp ticket_table x in
      Union (Left x)
  | Union (Right x) ->
      let x = to_runtime_ticket temp ticket_table x in
      Union (Right x)
  | List (x, tag) ->
      let x =
        List.fold_left
          (fun l x ->
            let x = to_runtime_ticket temp ticket_table x in
            x :: l)
          [] x
      in

      List (x, tag)
  | Option (Some x) ->
      let x = to_runtime_ticket temp ticket_table x in
      Option (Some x)
  | Option None -> Option None
  | Unit -> Unit
  | Map m ->
      let x =
        Map.fold
          (fun k v acc ->
            let k = to_runtime_ticket temp ticket_table k in
            let v = to_runtime_ticket temp ticket_table v in
            Map.add k v acc)
          m Map.empty
      in
      Map x
  | Closure _ -> raise (Ticket_table.Table `Ticket_doesnt_exist)
  | Ticket { ticket_id; amount } ->
      let () = take temp ticket_id (Deku_concepts.Amount.to_n amount) in
      Ticket_handle
        (Ticket_table.add ticket_table
           Ticket_table.Entry.
             {
               ticket_id;
               alive = true;
               amount = Deku_concepts.Amount.to_n amount;
             })
  | Ticket_handle _ -> raise (Ticket_table.Table `Ticket_doesnt_exist)
  | Set l ->
      let x =
        Set.fold
          (fun k acc ->
            let x = to_runtime_ticket temp ticket_table k in
            Set.add x acc)
          l Set.empty
      in
      Set x
