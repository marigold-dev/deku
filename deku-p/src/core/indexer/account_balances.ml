open Deku_ledger
open Deku_concepts
open Deku_stdlib
open Deku_protocol
open Eio

module Types = struct
  module Account : Rapper.CUSTOM with type t = Address.t = struct
    type t = Address.t

    let t =
      let encode account = Ok (Address.to_b58 account) in
      let decode str =
        Address.of_b58 str
        |> Option.to_result ~none:(Format.sprintf "invalid account: %s" str)
      in
      Caqti_type.(custom ~encode ~decode Caqti_type.string)
  end

  module Ticketer : Rapper.CUSTOM with type t = Ticket_id.ticketer = struct
    type t = Ticket_id.ticketer

    let t =
      let encode ticketer = Ok (Ticket_id.ticketer_to_string ticketer) in
      let decode str =
        Ticket_id.ticketer_of_string str
        |> Option.to_result ~none:(Format.sprintf "invalid account: %s" str)
      in
      Caqti_type.(custom ~encode ~decode Caqti_type.string)
  end

  module Amount : Rapper.CUSTOM with type t = Amount.t = struct
    type t = Amount.t

    let t =
      let encode amount = Ok (Amount.to_n amount |> N.to_z |> Z.to_int64) in
      let decode int64 =
        Z.of_int64 int64 |> N.of_z |> Option.map Amount.of_n
        |> Option.to_result
             ~none:(Format.sprintf "invalid amount: %s" (Int64.to_string int64))
      in

      (* TODO: this breaks when n > max_int_64. *)
      Caqti_type.(custom ~encode ~decode Caqti_type.int64)
  end
end

let init =
  [%rapper
    execute
      {sql|
        CREATE TABLE IF NOT EXISTS account_balances (
              account character(36) PRIMARY KEY,
              ticketer character(36) not null,
              data BYTEA not null,
              amount NUMERIC not null,
              UNIQUE (account, ticketer, data)
        )
        |sql}]

(* let request =
     let columns_tuple = String.concat ", " columns in
     let q =
       let open Caqti_query in
       S[L(sprintf "INSERT INTO %s (%s) VALUES (" table columns_tuple);
         concat ", " (List.mapi (fun i _ -> P i) columns); L")"]
     in
     Caqti_request.create row_type Caqti_type.unit Caqti_mult.zero (fun _ -> q)
   in
*)
let upsert_account_balance_query =
  let open Types in
  (* TODO: this will fail with large ticket amounts because int64 *)
  [%rapper
    execute
      {sql|
          INSERT INTO account_balances
          (account, ticketer, data, amount)
          VALUES
            (%Account{account}, %Ticketer{ticketer}, %string{data}, %Amount{amount})    
      |sql}]

let upsert_account_balance ~account ~ticketer ~data ~amount pool =
  let data = Bytes.to_string data in
  Caqti_eio.Pool.use
    (upsert_account_balance_query ~account ~ticketer ~data ~amount)
    pool
  |> Promise.await

module Account_balances_set = Stdlib.Set.Make (struct
  type t = Address.t * Ticket_id.t

  let compare a b =
    match Address.compare (fst a) (fst b) with
    | 0 -> Ticket_id.compare (snd a) (snd b)
    | result -> result
end)

let on_block ~state ~operations:_ ~receipts pool =
  Format.printf "iterating over %d receipts\n%!" (List.length receipts);
  let (Protocol.Protocol { ledger; _ }) = state in
  let modified = ref Account_balances_set.empty in
  Fiber.all
  @@ List.map
       (fun receipt ->
         let open Receipt in
         let yield_update account ticket_id =
           match Account_balances_set.mem (account, ticket_id) !modified with
           | false ->
               let amount = Ledger.balance account ticket_id ledger in
               [ (account, ticket_id, amount) ]
           | true ->
               modified :=
                 Account_balances_set.add (account, ticket_id) !modified;
               []
         in
         let updates =
           match receipt with
           | Ticket_transfer_receipt { sender; receiver; ticket_id; _ } ->
               yield_update sender ticket_id @ yield_update receiver ticket_id
           | Withdraw_receipt { account; ticket_id; _ } ->
               yield_update account ticket_id
           | _ -> []
         in
         fun () ->
           List.iter
             (fun (account, ticket_id, amount) ->
               let (Ticket_id.Ticket_id { ticketer; data }) = ticket_id in
               match
                 upsert_account_balance ~account ~ticketer ~data ~amount pool
               with
               | Ok () -> ()
               | Error err ->
                   Format.eprintf "%a\n%!" Caqti_error.pp err;
                   (* TODO: how to handle this error? *)
                   raise (Caqti_error.Exn err))
             updates)
       receipts
