#import "errors.mligo" "Errors"
#import "parameter.mligo" "Parameter"
#import "assert.mligo" "Assert"

type blake2b = Parameter.Types.blake2b
type vault = Parameter.Types.vault
type vault_handle_proof = Parameter.Types.vault_handle_proof
type vault_handle_structure = Parameter.Types.vault_handle_structure
type vault_withdraw = Parameter.Types.vault_withdraw
type vault_storage = Parameter.Types.vault_storage

let vault_check_handle_proof
  (proof: vault_handle_proof)
  (root: blake2b)
  (handle: vault_handle_structure) =
    let bit_is_set (bit: int) =
      Bitwise.and (Bitwise.shift_left 1n (abs bit)) handle.id <> 0n in
    let rec verify
      (bit, proof, parent: int * vault_handle_proof * blake2b): unit =
        match proof with
        | [] -> 
          let calculated_hash = Crypto.blake2b (Bytes.pack handle) in
          Assert.assert_msg (Errors.invalid_handle_data, parent = calculated_hash)
        | (left, right) :: tl ->
          let () = 
            let calculated_hash = Crypto.blake2b (Bytes.concat left right) in
            Assert.assert_msg (Errors.invalid_proof_hash, parent = calculated_hash) in
          verify (bit - 1, tl, (if bit_is_set bit then right else left)) in
    (* each bit in the handle_id indicates if we should go left or right in
        the proof, but the first bit to check is the MSB, so we use the length
        of the proof to know what is the MSB *)
    let most_significant_bit = int (List.length proof) - 1 in
    verify (most_significant_bit, proof, root)


  let vault_withdraw (withdraw: vault_withdraw) (storage: vault_storage) =
    let handles_hash = withdraw.handles_hash in
    let handle = withdraw.handle in
    let proof = withdraw.proof in

    let { known_handles_hash; used_handles; vault } = storage in

    let () = Assert.assert_msg (
      Errors.unknown_handle_hash,
      Big_map.mem handles_hash known_handles_hash
    ) in
    let () = Assert.assert_msg (
      Errors.used_hash,
      not Big_map.mem handle.id used_handles
    ) in
    let () = Assert.assert_msg (
      Errors.owner_withdraw_handle,
      handle.owner = Tezos.sender
    ) in
    let () = vault_check_handle_proof proof handles_hash handle in

    (* start transfer *)
    let used_handles = Big_map.add handle.id () used_handles in

    let (fragment, vault) =
      let (old_ticket, vault) =
        match 
          Big_map.get_and_update
            (handle.ticketer, handle.data)
            (None: bytes ticket option)
            vault
        with
        | (Some old_ticket, vault) -> (old_ticket, vault)
        | (None, _) -> (failwith Errors.unreachable : bytes ticket * vault) in
      let ((_, (_, total)), old_ticket) = Tezos.read_ticket old_ticket in
      let (fragment, remaining) =
        match
          Tezos.split_ticket
            old_ticket
            (handle.amount, abs (total - handle.amount))
        with
        | Some (fragment, remaining) -> (fragment, remaining)
        | None -> (failwith Errors.unreachable : bytes ticket * bytes ticket) in
      (fragment, Big_map.add (handle.ticketer, handle.data) remaining vault) in
    let transaction = Tezos.transaction fragment 0tz withdraw.callback in
    ([transaction], {
      known_handles_hash = known_handles_hash;
      used_handles = used_handles;
      vault = vault;
    })