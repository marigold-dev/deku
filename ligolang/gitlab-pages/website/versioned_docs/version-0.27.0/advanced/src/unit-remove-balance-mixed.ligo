(*PART 0*)
#include "remove-balance.ligo"
const _u = Test.reset_state (5n, (list [] : list (nat)))

(*PART 1*)
const balances : balances = block {
  const a1 = Test.nth_bootstrap_account(1);
  const a2 = Test.nth_bootstrap_account(2);
  const a3 = Test.nth_bootstrap_account(3); } with
  (map [ a1 -> 10tz ; a2 -> 100tz ; a3 -> 1000tz ])

(*PART 2*)
const test =
  List.iter (
    (function (const threshold : tez ; const expected_size : nat) is
      block {
        function tester(const input : (balances * tez)) is Map.size(balances_under(input.0, input.1));
        const size_ = Test.run(tester, (balances, threshold));
        const expected_size = Test.eval(expected_size);
        Test.log (("expected", expected_size));
        Test.log (("actual", size_));
      } with
        assert (Test.michelson_equal (size_, expected_size))),
    list [(15tez, 2n); (130tez, 1n); (1200tez, 0n)])
