[@warning "-26"];
open Setup;
open Protocol;

describe("protocol state", ({test, _}) => {
  let ticket = {
    open Tezos_interop;
    let random_hash =
      Mirage_crypto_rng.generate(20)
      |> Cstruct.to_string
      |> Helpers.BLAKE2B_20.of_raw_string
      |> Option.get;
    Ticket.{
      ticketer: Originated({contract: random_hash, entrypoint: None}),
      data: Bytes.of_string(""),
    };
  };
  let make_state = (~validators=?, ()) => {
    let (key_wallet, wallet) = Wallet.make_wallet();
    let state = {
      ...make(~initial_block=Block.genesis),
      ledger:
        Ledger.empty |> Ledger.deposit(wallet, Amount.of_int(500), ticket),
    };
    let validators = {
      open Helpers;
      let.default () =
        state.validators
        |> Validators.add({address: Wallet.get_pub_key(key_wallet)});
      validators;
    };
    let state = {...state, validators};
    (state, key_wallet, wallet);
  };

  let apply_block = (~author=?, ~main=[], ~side=[], state) => {
    let author = {
      open Helpers;
      let.default () = {
        let validator = state.validators |> Validators.current |> Option.get;
        validator.address;
      };
      author;
    };
    let state = Protocol.make(~initial_block=Block.genesis);
    let block =
      Block.produce(
        ~state,
        ~author,
        ~main_chain_ops=main,
        ~side_chain_ops=side,
      );
    apply_block(state, block);
  };
  let test_wallet_offset = (name, ~free_diff_a=0, ~free_diff_b=0, f) =>
    test(
      name,
      ({expect, _}) => {
        let expect_amount = (left, right) =>
          expect.int(Amount.to_int(left)).toBe(right);
        // TODO: use random wallet with random amount
        let (old_state, key_a, wallet_a) = make_state();
        let (key_b, wallet_b) = Wallet.make_wallet();
        let new_state = f(old_state, (wallet_a, key_a), (wallet_b, key_b));

        expect_amount(
          Ledger.balance(wallet_a, ticket, old_state.ledger),
          500,
        );
        expect_amount(Ledger.balance(wallet_b, ticket, old_state.ledger), 0);

        // TODO: test that it changes only the target wallet
        expect_amount(
          Ledger.balance(wallet_a, ticket, new_state.ledger),
          500 + free_diff_a,
        );
        expect_amount(
          Ledger.balance(wallet_b, ticket, new_state.ledger),
          0 + free_diff_b,
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
  // transaction
  test_wallet_offset(
    "transaction",
    ~free_diff_a=-7,
    ~free_diff_b=7,
    (state, (source, secret), (destination, _)) => {
      let (state, result) =
        apply_side_chain(
          state,
          Operation.Side_chain.sign(
            ~secret,
            ~nonce=0l,
            ~block_height=0L,
            ~source,
            ~kind=
              Transaction({destination, amount: Amount.of_int(7), ticket}),
          ),
        );
      assert(result == `Transaction);
      state;
    },
  );
  test_failed_wallet_offset(
    "transaction",
    "not enough funds",
    (state, (source, secret), (destination, _)) => {
    let (state, result) =
      apply_side_chain(
        state,
        Operation.Side_chain.sign(
          ~secret,
          ~nonce=0l,
          ~block_height=0L,
          ~source,
          ~kind=
            Transaction({
              destination,

              amount: Amount.of_int(501),
              ticket,
            }),
        ),
      );
    assert(result == `Transaction);
    state;
  });
  test("validators", ({expect, _}) => {
    // TODO: this clearly should be splitten and properly automated
    let (state, _, _) = make_state(~validators=Validators.empty, ());
    let validators = state.validators;
    expect.option(Validators.current(validators)).toBeNone();
    expect.list(Validators.to_list(validators)).toBeEmpty();
    // next
    // let state = Protocol.next(state);
    // let validators = state.validators;
    // expect.bool(Validators.current(validators) == Some(another_validator)).
    //   toBeTrue();
    // expect.list(Validators.validators(validators)).toEqual([
    //   new_validator,
    //   another_validator,
    // ]);
    // remove current validator
    // next
    // let state = Protocol.next(state);
    // let validators = state.validators;
    // expect.bool(Validators.current(validators) == Some(new_validator)).
    //   toBeTrue();
    // expect.list(Validators.validators(validators)).toEqual([new_validator]);
    // remove all validators
  });
  // TODO: check on of_yojson
  /*
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
   */
});
