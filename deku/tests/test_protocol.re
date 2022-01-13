[@warning "-26"];
open Setup;
open Protocol;

describe("protocol state", ({test, _}) => {
  let ticket = {
    open Crypto;
    let random_hash =
      Random.generate(20)
      |> Cstruct.to_string
      |> BLAKE2B_20.of_raw_string
      |> Option.get;
    Ticket_id.{
      ticketer: Originated({contract: random_hash, entrypoint: None}),
      data: Bytes.of_string(""),
    };
  };
  let make_state = (~validators=?, ()) => {
    let (key_wallet, address) = Address.Implicit.make();
    let state = {
      ...make(~initial_block=Block.genesis),
      ledger:
        Ledger.empty |> Ledger.deposit(address, Amount.of_int(500), ticket),
    };
    let validators = {
      open Helpers;
      let.default () = state.validators |> Validators.add({address: address});
      validators;
    };
    let state = {...state, validators};
    (state, key_wallet, address);
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
        let (old_state, key_a, address_a) = make_state();
        let (key_b, address_b) = Address.Implicit.make();
        let new_state =
          f(old_state, (address_a, key_a), (address_b, key_b));

        expect_amount(
          Ledger.balance(address_a, ticket, old_state.ledger),
          500,
        );
        expect_amount(
          Ledger.balance(address_b, ticket, old_state.ledger),
          0,
        );

        // TODO: test that it changes only the target wallet
        expect_amount(
          Ledger.balance(address_a, ticket, new_state.ledger),
          500 + free_diff_a,
        );
        expect_amount(
          Ledger.balance(address_b, ticket, new_state.ledger),
          0 + free_diff_b,
        );
      },
    );
  let test_failed_wallet_offset = (name, expected_message, f) =>
    test_wallet_offset(
      name ++ " " ++ expected_message, (state, address_a, address_b) =>
      try({
        let _state = f(state, address_a, address_b);
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
              Transaction({
                destination: Implicit(destination),
                parameter: failwith("todo"),
                entrypoint: None,
              }),
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
              destination: Implicit(destination),
              parameter: failwith("todo"),
              entrypoint: None,
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
  test("contract origination and invocation", _ => {
    let (state, secret, source) = make_state();
    let (key_b, address_b) = Address.Implicit.make();
    open Operation.Side_chain;
    open Interpreter.Types;
    open Zinc;
    let sign = (kind, i) =>
      sign(~secret, ~nonce=i, ~block_height=0L, ~source, ~kind);

    let contract = [
      (
        "main",
        [
          Core(Grab),
          Core(Access(0)),
          Core(Grab),
          Core(Access(0)),
          Core(Grab),
          Core(Access(0)),
          Adt(RecordAccess(1)),
          Core(Grab),
          Core(Access(1)),
          Adt(RecordAccess(0)),
          Core(Grab),
          Core(Access(0)),
          Core(Access(1)),
          Operation(Add),
          Plain_old_data(Nil),
          Adt(MakeRecord(2)),
          Core(Return),
        ],
      ),
    ];
    let storage = Stack_item.Z(Plain_old_data(Num(Z.zero)));
    let parameter = Stack_item.Z(Plain_old_data(Num(Z.one)));
    let origination = sign(Originate_contract((contract, storage)), 0l);
    let contract_address =
      origination
      |> to_contract_hash
      |> Protocol.Address.Originated.of_contract_hash;
    let invocation =
      Invocation({
        parameter,
        destination: Protocol.Address.(contract_address |> of_originated),
        entrypoint: Some("main"),
      });
    let (state, result) = apply_side_chain(state, origination);
    assert(result == `Origination);
    let (state, result) = apply_side_chain(state, sign(invocation, 1l));
    assert(result == `Invocation);
    let (state, result) = apply_side_chain(state, sign(invocation, 2l));
    assert(result == `Invocation);
    switch (
      Protocol.Contract_storage.(
        contract_address |> get(state.contracts_storage)
      )
    ) {
    | None =>
      failwith("Contract wasn't in contract storage, it should have been")
    | Some(contract) =>
      assert(
        Stack_item.equal(
          contract.storage,
          Stack_item.Z(Plain_old_data(Num(Z.of_int(2)))),
        ),
      )
    };
    ();
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
