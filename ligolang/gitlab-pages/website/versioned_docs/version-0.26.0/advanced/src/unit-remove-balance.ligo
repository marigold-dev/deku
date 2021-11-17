// PART 0
const under_test : option (string) = Some ("./gitlab-pages/docs/advanced/src/remove-balance.ligo")
const _u = Test.reset_state (5n, (list [] : list (nat)))

// PART 1
function bs_addr (const i : int) is Test.compile_value (Test.nth_bootstrap_account (i))

const balances : michelson_program =
  Test.compile_expression_subst
    ( under_test,
      [%pascaligo ({| map [ ( $a1 : address) -> 10tz ; ( $a2 : address) -> 100tz ; ( $a3 : address) -> 1000tz ] |} : ligo_program)],
      list [("a1", bs_addr (1)); ("a2", bs_addr (2)); ("a3", bs_addr (3))] )

// PART 2
function to_tez (const i : nat) is
  Test.compile_expression_subst
    ( (None : option (string)),
      [%pascaligo ({| $i * 1tez |} : ligo_program)],
      list [("i", Test.compile_value (i))] )

const test =
  List.iter (
    (function (const threshold : nat ; const expected_size : nat) is
      block {
        const expected_size = Test.compile_value (expected_size);
        const size_ = Test.compile_expression_subst
          ( under_test,
            [%pascaligo ({| Map.size (balances_under ($b, $threshold)) |} : ligo_program)],
            list [("b", balances); ("threshold", to_tez (threshold))]
          );
        Test.log (("expected", expected_size));
        Test.log (("actual", size_));
      } with
        assert (Test.michelson_equal (size_, expected_size))),
    list [(15n, 2n); (130n, 1n); (1200n, 0n)])