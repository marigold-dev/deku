#include "../FA1.2.mligo"

let test_transfer =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = from_; spender = sender_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
  let () = Test.set_source sender_ in
  let () = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  assert ((Big_map.find_opt to_ new_storage.tokens = Some 110n) &&
          (Big_map.find_opt from_ new_storage.tokens = Some 90n) &&
          (Big_map.find_opt sender_ new_storage.tokens = Some 100n) &&
          (Big_map.find_opt ({ owner = from_; spender = sender_ }) new_storage.allowances = Some 90n) &&
          (new_storage.total_supply = 300n))

let test_transfer_not_e_allowance =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = from_; spender = sender_ }, 0n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
  let () = Test.set_source sender_ in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughAllowance"))
  | Fail Other -> failwith "Transaction should fail with rejection"

let test_transfer_not_e_balance =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 0n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = from_; spender = sender_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
  let () = Test.set_source sender_ in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughBalance"))
  | Fail Other -> failwith "Transaction should fail with rejection"

let test_approve =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = sender_; spender = from_ }, 0n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Approve { spender = from_; value = 100n } in
  let () = Test.set_source sender_ in
  let () = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
          (Big_map.find_opt from_ new_storage.tokens = Some 100n) &&
          (Big_map.find_opt sender_ new_storage.tokens = Some 100n) &&
          (Big_map.find_opt ({ owner = sender_; spender = from_ }) new_storage.allowances = Some 100n) &&
          (new_storage.total_supply = 300n))

let test_approve_unsafe =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = sender_; spender = from_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Approve { spender = from_; value = 100n } in
  let () = Test.set_source sender_ in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "UnsafeAllowanceChange"))
  | Fail Other -> failwith "Transaction should fail with rejection"

let test_get_allowance =
  let dummy_contract (v, s : nat * nat) : operation list * nat = ([] : operation list), v + s in
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let (dummy_typed_addr, _, _) = Test.originate dummy_contract 0n 0tez in
  let dummy_typed_contr = Test.to_contract dummy_typed_addr in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = from_; spender = sender_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = GetAllowance { request = { owner = from_; spender = sender_} ; callback = dummy_typed_contr } in
  let () = Test.set_source sender_ in
  let () = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt from_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt sender_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt ({ owner = from_; spender = sender_ }) new_storage.allowances = Some 100n) &&
                  (new_storage.total_supply = 300n)) in
  let dummy_new_storage = Test.get_storage dummy_typed_addr in
  assert (dummy_new_storage = 100n)

let test_get_balance =
  let dummy_contract (v, s : nat * nat) : operation list * nat = ([] : operation list), v + s in
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let (dummy_typed_addr, _, _) = Test.originate dummy_contract 0n 0tez in
  let dummy_typed_contr = Test.to_contract dummy_typed_addr in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = sender_; spender = from_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = GetBalance { owner = from_ ; callback = dummy_typed_contr } in
  let () = Test.set_source sender_ in
  let () = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt from_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt sender_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt ({ owner = sender_; spender = from_ }) new_storage.allowances = Some 100n) &&
                  (new_storage.total_supply = 300n)) in
  let dummy_new_storage = Test.get_storage dummy_typed_addr in
  assert (dummy_new_storage = 100n)

let test_get_total_supply =
  let dummy_contract (v, s : nat * nat) : operation list * nat = ([] : operation list), v + s in
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let (dummy_typed_addr, _, _) = Test.originate dummy_contract 0n 0tez in
  let dummy_typed_contr = Test.to_contract dummy_typed_addr in
  let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  allowances = Big_map.literal [({ owner = sender_; spender = from_ }, 100n)];
                  total_supply = 300n } in
  let (typed_addr, _, _) = Test.originate main storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = GetTotalSupply { callback = dummy_typed_contr; request = () } in
  let () = Test.set_source sender_ in
  let () = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt from_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt sender_ new_storage.tokens = Some 100n) &&
                  (Big_map.find_opt ({ owner = sender_; spender = from_ }) new_storage.allowances = Some 100n) &&
                  (new_storage.total_supply = 300n)) in
  let dummy_new_storage = Test.get_storage dummy_typed_addr in
  assert (dummy_new_storage = 300n)
