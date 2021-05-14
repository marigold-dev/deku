[@warning "-26"];
open Setup;
open Protocol;

describe("protocol state", ({test, _}) => {
  let make_address = () => {
    open Mirage_crypto_pk;
    let key = Rsa.generate(~bits=2048, ());
    Rsa.pub_of_priv(key);
  };
  let make_wallet = () => {
    open Mirage_crypto_pk;
    let key = Rsa.generate(~bits=2048, ());
    let t = Rsa.pub_of_priv(key);
    (key, Wallet.of_address(t));
  };
  let make_state = (~validators=?, ()) => {
    let (key_wallet, wallet) = make_wallet();
    let state = {
      ...make(~initial_block=Block.genesis),
      ledger:
        Ledger.empty
        |> Ledger.deposit(~destination=wallet, ~amount=Amount.of_int(1000))
        |> Ledger.unfreeze(~wallet, ~amount=Amount.of_int(500)),
    };
    let validators = {
      open Helpers;
      let.default () =
        state.validators
        |> Validators.add({
             address: Wallet.get_address(wallet),
             uri: Uri.of_string("http://localhost:8080"),
           });
      validators;
    };
    let state = {...state, validators};
    (state, key_wallet, wallet);
  };
  let self_sign_side = (~key, op) => {
    let Signed.{key, signature, data} = Signed.sign(~key, op);
    Operation.Side_chain.Self_signed.verify(~key, ~signature, data)
    |> Result.get_ok;
  };
  let apply_block = (~author=?, ~block_height=1, ~main=[], ~side=[], state) => {
    let author = {
      open Helpers;
      let.default () = {
        let validator = state.validators |> Validators.current |> Option.get;
        validator.address;
      };
      author;
    };
    let block =
      Block.make(
        ~previous_hash="tuturu",
        ~author,
        ~block_height=Int64.of_int(block_height),
        ~main_chain_ops=main,
        ~side_chain_ops=side,
      );
    apply_block(state, block);
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
        let (old_state, key_a, wallet_a) = make_state();
        let (key_b, wallet_b) = make_wallet();
        let new_state = f(old_state, (wallet_a, key_a), (wallet_b, key_b));

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
  test_wallet_offset("deposit", ~frozen_diff_a=3, (state, (wallet, _), _) =>
    apply_main_chain(
      state,
      Deposit({destination: wallet, amount: Amount.of_int(3)}),
    )
  );
  // withdraw
  test_wallet_offset("withdraw", ~frozen_diff_a=-4, (state, (source, _), _) =>
    apply_main_chain(state, Withdraw({source, amount: Amount.of_int(4)}))
  );
  test_failed_wallet_offset(
    "withdraw", "not enough funds", (state, (source, _), _) =>
    apply_main_chain(state, Withdraw({source, amount: Amount.of_int(501)}))
  );
  // freeze
  test_wallet_offset(
    "freeze", ~free_diff_a=-5, ~frozen_diff_a=5, (state, (source, key), _) =>
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(5),
          ~kind=Freeze,
        ),
      ),
    )
  );
  test_failed_wallet_offset(
    "freeze", "not enough funds", (state, (source, key), _) =>
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(501),
          ~kind=Freeze,
        ),
      ),
    )
  );
  // unfreeze
  // TODO: is unfreeze a good idea?
  test_wallet_offset(
    "unfreeze", ~free_diff_a=6, ~frozen_diff_a=-6, (state, (source, key), _) => {
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(6),
          ~kind=Unfreeze,
        ),
      ),
    )
  });
  test_failed_wallet_offset(
    "unfreeze", "not enough funds", (state, (source, key), _) =>
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(501),
          ~kind=Unfreeze,
        ),
      ),
    )
  );
  // transaction
  test_wallet_offset(
    "transaction",
    ~free_diff_a=-7,
    ~free_diff_b=7,
    (state, (source, key), (destination, _)) => {
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(7),
          ~kind=Transaction({destination: destination}),
        ),
      ),
    )
  });
  test_failed_wallet_offset(
    "transaction",
    "not enough funds",
    (state, (source, key), (destination, _)) =>
    apply_side_chain(
      state,
      self_sign_side(
        ~key,
        Operation.Side_chain.make(
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~amount=Amount.of_int(501),
          ~kind=Transaction({destination: destination}),
        ),
      ),
    )
  );
  test("validators", ({expect, _}) => {
    // TODO: this clearly should be splitten and properly automated
    let (state, _, _) = make_state(~validators=Validators.empty, ());
    let validators = state.validators;
    expect.option(Validators.current(validators)).toBeNone();
    expect.list(Validators.validators(validators)).toBeEmpty();
    let new_validator =
      Validators.{
        address: make_address(),
        uri: Uri.of_string("http://localhost:1234"),
      };
    let state =
      apply_main_chain(
        state,
        Operation.Main_chain.Add_validator(new_validator),
      );
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(new_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([new_validator]);
    // duplicated is a noop
    let state =
      apply_main_chain(
        state,
        Operation.Main_chain.Add_validator(new_validator),
      );
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(new_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([new_validator]);
    // additional shouldn't move current
    let another_validator =
      Validators.{
        address: make_address(),
        uri: Uri.of_string("http://localhost:12345"),
      };
    let state =
      apply_main_chain(
        state,
        Operation.Main_chain.Add_validator(another_validator),
      );
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(new_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([
      new_validator,
      another_validator,
    ]);
    // next
    let state = Protocol.next(state);
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(another_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([
      new_validator,
      another_validator,
    ]);
    // remove current validator
    let state =
      apply_main_chain(
        state,
        Operation.Main_chain.Remove_validator(another_validator),
      );
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(new_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([new_validator]);
    // next
    let state = Protocol.next(state);
    let validators = state.validators;
    expect.bool(Validators.current(validators) == Some(new_validator)).
      toBeTrue();
    expect.list(Validators.validators(validators)).toEqual([new_validator]);
    // remove all validators
    let state =
      apply_main_chain(
        state,
        Operation.Main_chain.Remove_validator(new_validator),
      );
    let validators = state.validators;
    expect.option(Validators.current(validators)).toBeNone();
    expect.list(Validators.validators(validators)).toBeEmpty();
  });
  test("invalid block height", _ => {
    let (state, _, _) = make_state();
    let state =
      switch (apply_block(~block_height=1, state)) {
      | Ok(state) => state
      | Error(`Invalid_block_when_applying) => assert(false)
      };
    switch (apply_block(~block_height=1, state)) {
    | Ok(_) => assert(false)
    | Error(`Invalid_block_when_applying) => ()
    };
  });
});
