open Helpers
open Crypto

(* - Constructing a large deku state to test state hashing is too awkward.
   - The only parts of the Deku state that can be large are the ledger
   and the contract storage: `src/core_deku/state.ml`

   type t = {
     ledger : Ledger.t;
     contract_storage: Contract_storage.t
   }

   + Ledger.ml:

   type t = {
     ledger : Address_and_ticket_map.t;
     withdrawal_handles: Withdrawal_handle_tree.t
   }

   And Address_and_ticket_map.t of type:

    type t = Amount.t Map.t

     ++ Amount: type t = int

     ++ Map: type t = key

        type key = {
          address: Key_hash.t;
          ticket: Ticket_id.t
        }

   + Contract_storage.ml:

   type t = Contract.t Map.t [@@deriing yojson, eq]

      ++ Contract_vm.ml:
         Contract.t :
           type t = {
             code: Lamda_vm.Ir.code;
             storage: Lamda_vm.Ir.value
           }

      ++ Map: Map.Make_with_yojson (Contract_address)
        Contract_address: type t = BLAKE2B_20.t

   - Both are hash maps, so this is a good substitute.
   - This is an easy way to test the speed of a bigmap -> yojson -> BLAKE2B function.
*)

module Map = Map.Make_with_yojson (struct
  type t = int [@@deriving yojson]

  let compare = Int.compare
end)

(* Small maps have ints as keys and strings as values *)
let small_map index =
  let rec go index map =
    if index = 0 then
      map
    else
      go (index - 1) (Map.add index (BLAKE2B.hash @@ Int.to_string index) map)
  in
  go index Map.empty

(* Big map has ints as keys and small maps as values*)
let big_map index small_map =
  let rec go index map =
    if index = 0 then
      map
    else
      go (index - 1) (Map.add index small_map map) in
  go index Map.empty

let generate_maps bmap_size smap_size =
  let smap = small_map smap_size in
  big_map bmap_size smap

(* Hashes the json of a map of size bmap_size,
   with values that are each maps of size smap_size *)
let hash_the_map ~bmap_size ~smap_size =
  let map = generate_maps bmap_size smap_size in
  Map.to_yojson (Map.to_yojson BLAKE2B.to_yojson) map
  |> Yojson.Safe.to_string
  |> BLAKE2B.hash
