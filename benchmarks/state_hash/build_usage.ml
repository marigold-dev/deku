(*********************************************************************************)
(* Tezos address *)

let make_tezos_address () =
  let open Tezos in
  let _key, address = Crypto.Ed25519.generate () in
  let hash = Crypto.Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

(* Generate n Tezos addresses *)
let make_n_tezos_address size =
  List.fold_left
    (fun result _ ->
      let tezos_address = make_tezos_address () in
      tezos_address :: result)
    []
    (Core.List.range 0 (size - 1))

(*********************************************************************************)
(* Convert Tezos address to Deku address *)

let make_deku_address tezos_address =
  tezos_address
  |> Tezos.Address.to_string
  |> Crypto.Key_hash.of_string
  |> Option.get

let make_n_deku_addresses tezos_addresses =
  List.fold_left
    (fun result tezos_address ->
      let deku_add = make_deku_address tezos_address in
      deku_add :: result)
    [] tezos_addresses

(*let make_address () =
    let _secret, _key, key_hash = Crypto.Key_hash.make_ed25519 () in
    key_hash

  (* Generate n Deku addresses*)
  let make_n_address size : Crypto.Key_hash.t list =
    List.fold_left
      (fun result _ ->
        let key_hash = make_address () in
        key_hash :: result)
      []
      (Core.List.range 0 (size - 1))*)

let print_address add =
  Printf.printf "Deku address: %s \n" (Crypto.Key_hash.to_string add)

let print_addresses l = List.iter (fun add -> print_address add) l

(*********************************************************************************)
(* Ticket *)

let make_ticket ?ticketer ?data () : Tezos.Ticket_id.t =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Crypto.Random.generate 20
        |> Cstruct.to_string
        |> Crypto.BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Crypto.Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

(* Generate n numbers of tickets *)
let make_n_tickets size : Tezos.Ticket_id.t list =
  List.fold_left
    (fun result _i ->
      let ticket_id = make_ticket () in
      ticket_id :: result)
    []
    (Core.List.range 0 (size - 1))
