open Setup;
open Protocol;

describe("protocol state", ({test, _}) => {
  open Protocol_state;

  let make_wallet = () => {
    open Mirage_crypto_pk;
    let key = Rsa.generate(~bits=2048, ());
    let t = Rsa.pub_of_priv(key);
    (key, Wallet.of_address(t));
  };
  let make_state = () => {
    let (key_wallet, wallet) = make_wallet();
    let state = {
      ...Protocol_state.empty,
      ledger:
        Ledger.empty
        |> Ledger.deposit(~destination=wallet, ~amount=Amount.of_int(1000))
        |> Ledger.unfreeze(~wallet, ~amount=Amount.of_int(500)),
    };
    (state, key_wallet, wallet);
  };
  let test_wallet_offset =
      (
        name,
        ~free_diff_a=0,
        ~frozen_diff_a=0,
        ~free_diff_b=0,
        ~frozen_diff_b=0,
        f,
      ) =>
    test(
      name,
      ({expect, _}) => {
        let expect_amount = (left, right) =>
          expect.int(Amount.to_int(left)).toBe(right);
        // TODO: use random wallet with random amount
        let (old_state, _, wallet_a) = make_state();
        let (_, wallet_b) = make_wallet();
        let new_state = f(old_state, wallet_a, wallet_b);

        expect_amount(Ledger.get_free(wallet_a, old_state.ledger), 500);
        expect_amount(Ledger.get_frozen(wallet_a, old_state.ledger), 500);
        expect_amount(Ledger.get_free(wallet_b, old_state.ledger), 0);
        expect_amount(Ledger.get_frozen(wallet_b, old_state.ledger), 0);

        // TODO: test that it changes only the target wallet
        expect_amount(
          Ledger.get_free(wallet_a, new_state.ledger),
          500 + free_diff_a,
        );
        expect_amount(
          Ledger.get_frozen(wallet_a, new_state.ledger),
          500 + frozen_diff_a,
        );
        expect_amount(
          Ledger.get_free(wallet_b, new_state.ledger),
          0 + free_diff_b,
        );
        expect_amount(
          Ledger.get_frozen(wallet_b, new_state.ledger),
          0 + frozen_diff_b,
        );
      },
    );
  let test_failed_wallet_offset = (name, expected_message, f) =>
    test_wallet_offset(
      name ++ " " ++ expected_message, (state, wallet_a, wallet_b) =>
      try({
        let _state = f(state, wallet_a, wallet_b);
        assert(false);
      }) {
      | Noop(message) =>
        assert(message == expected_message);
        state;
      }
    );
  // TODO: should deposit add to frozen?
  test_wallet_offset("deposit", ~frozen_diff_a=3, (state, wallet, _) =>
    apply_main_chain(
      state,
      Deposit({destination: wallet, amount: Amount.of_int(3)}),
    )
  );
  // withdraw
  test_wallet_offset("withdraw", ~frozen_diff_a=-4, (state, source, _) =>
    apply_main_chain(state, Withdraw({source, amount: Amount.of_int(4)}))
  );
  test_failed_wallet_offset(
    "withdraw", "not enough funds", (state, source, _) =>
    apply_main_chain(state, Withdraw({source, amount: Amount.of_int(501)}))
  );
  // freeze
  test_wallet_offset(
    "freeze", ~free_diff_a=-5, ~frozen_diff_a=5, (state, wallet, _) =>
    apply_side_chain(
      state,
      Freeze({
        nonce: 0l,
        block_height: 0L,
        wallet,
        amount: Amount.of_int(5),
      }),
    )
  );
  test_failed_wallet_offset("freeze", "not enough funds", (state, wallet, _) =>
    apply_side_chain(
      state,
      Freeze({
        nonce: 0l,
        block_height: 0L,
        wallet,
        amount: Amount.of_int(501),
      }),
    )
  );
  // unfreeze
  // TODO: is unfreeze a good idea?
  test_wallet_offset(
    "unfreeze", ~free_diff_a=6, ~frozen_diff_a=-6, (state, wallet, _) => {
    apply_side_chain(
      state,
      Unfreeze({
        nonce: 0l,
        block_height: 0L,
        wallet,
        amount: Amount.of_int(6),
      }),
    )
  });
  test_failed_wallet_offset(
    "unfreeze", "not enough funds", (state, wallet, _) =>
    apply_side_chain(
      state,
      Unfreeze({
        nonce: 0l,
        block_height: 0L,
        wallet,
        amount: Amount.of_int(501),
      }),
    )
  );
  // transaction
  test_wallet_offset(
    "transaction",
    ~free_diff_a=-7,
    ~free_diff_b=7,
    (state, source, destination) => {
    apply_side_chain(
      state,
      Transaction({
        nonce: 0l,
        block_height: 0L,
        source,
        destination,
        amount: Amount.of_int(7),
      }),
    )
  });
  test_failed_wallet_offset(
    "transaction", "not enough funds", (state, source, destination) =>
    apply_side_chain(
      state,
      Transaction({
        nonce: 0l,
        block_height: 0L,
        source,
        destination,
        amount: Amount.of_int(501),
      }),
    )
  );
});
