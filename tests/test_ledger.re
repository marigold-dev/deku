open Setup;
open Helpers;
open Crypto;
open Core;
open Ledger;

describe("ledger", ({test, _}) => {
  // TODO: maybe have a "total amount" function to ensure invariant?
  let make_ticket = (~ticketer=?, ~data=?, ()) => {
    open Tezos;

    let ticketer =
      switch (ticketer) {
      | Some(ticketer) => ticketer
      | None =>
        let random_hash =
          Random.generate(20)
          |> Cstruct.to_string
          |> BLAKE2B_20.of_raw_string
          |> Option.get;
        Address.Originated({contract: random_hash, entrypoint: None});
      };
    let data =
      switch (data) {
      | Some(data) => data
      | None => Random.generate(256) |> Cstruct.to_bytes
      };
    Ticket_id.{ticketer, data};
  };
  let make_address = () => snd(Address.Implicit.make());
  let make_tezos_address = () => {
    open Crypto;
    open Tezos;
    let (_key, address) = Ed25519.generate();
    let hash = Ed25519.Key_hash.of_key(address);
    Address.Implicit(Ed25519(hash));
  };
  let setup_two = () => {
    let t1 = make_ticket();
    let t2 = make_ticket();

    let a = make_address();
    let b = make_address();
    let t =
      empty
      |> deposit(a, Amount.of_int(100), t1)
      |> deposit(a, Amount.of_int(300), t2)
      |> deposit(b, Amount.of_int(200), t1)
      |> deposit(b, Amount.of_int(400), t2);
    (t, (t1, t2), (a, b));
  };
  let test = (name, f) =>
    test(
      name,
      ({expect, _}) => {
        let expect_balance = (address, ticket, expected, t) => {
          expect.equal(
            Amount.of_int(expected),
            balance(address, ticket, t),
          );
        };
        f(expect, expect_balance);
      },
    );
  test("amount", (expect, _) => {
    expect.equal(Amount.(of_int(0) + of_int(5)), Amount.of_int(5));
    expect.fn(() => Amount.of_int(-1)).toThrowException(
      Invalid_argument("Negative amount"),
    );
  });
  test("balance", (_, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();
    expect_balance(a, t1, 100, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 400, t);

    expect_balance(make_address(), t1, 0, t);
    expect_balance(make_address(), t2, 0, t);
    expect_balance(a, make_ticket(), 0, t);
    expect_balance(b, make_ticket(), 0, t);
  });
  test("transfer", (expect, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();
    let c = make_address();

    let t = transfer(~sender=a, ~destination=b, Amount.of_int(1), t1, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 400, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 0, t);

    let t = transfer(~sender=b, ~destination=a, Amount.of_int(3), t2, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 397, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 0, t);

    let t = transfer(~sender=b, ~destination=c, Amount.of_int(5), t2, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 392, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 5, t);

    let t = transfer(~sender=a, ~destination=c, Amount.of_int(99), t1, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 0, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 392, t);
    expect_balance(c, t1, 99, t);
    expect_balance(c, t2, 5, t);

    {
      let t = transfer(~sender=b, ~destination=c, Amount.of_int(202), t1, t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      let d = make_address();
      let t = transfer(~sender=d, ~destination=c, Amount.of_int(1), t2, t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      let t3 = make_ticket();
      let t = transfer(~sender=a, ~destination=b, Amount.of_int(1), t3, t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    ();
  });
  test("deposit", (_, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();

    let t = deposit(a, Amount.of_int(123), t1, t);
    expect_balance(a, t1, 223, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 400, t);

    let t = deposit(b, Amount.of_int(456), t2, t);
    expect_balance(a, t1, 223, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 856, t);
  });
  test("withdraw", (expect, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();
    let destination = make_tezos_address();

    let t = withdraw(~sender=a, ~destination, Amount.of_int(10), t1, t);
    expect.result(t).toBeOk();
    let (t, handle) = Result.get_ok(t);
    expect_balance(a, t1, 90, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 400, t);
    expect.equal(handle.id, 0);
    expect.equal(handle.owner, destination);
    expect.equal(handle.amount, Amount.of_int(10));

    let t = withdraw(~sender=b, ~destination, Amount.of_int(9), t2, t);
    expect.result(t).toBeOk();
    let (t, handle) = Result.get_ok(t);
    expect_balance(a, t1, 90, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 391, t);
    expect.equal(handle.id, 1);
    expect.equal(handle.owner, destination);
    expect.equal(handle.amount, Amount.of_int(9));

    let t = withdraw(~sender=a, ~destination, Amount.of_int(8), t2, t);
    expect.result(t).toBeOk();
    let (t, handle) = Result.get_ok(t);
    expect_balance(a, t1, 90, t);
    expect_balance(a, t2, 292, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 391, t);
    expect.equal(handle.id, 2);
    expect.equal(handle.owner, destination);
    expect.equal(handle.amount, Amount.of_int(8));

    {
      let t = withdraw(~sender=a, ~destination, Amount.of_int(91), t1, t);
      expect.result(t).toBeError();
    };
    {
      let t = withdraw(~sender=b, ~destination, Amount.of_int(203), t1, t);
      expect.result(t).toBeError();
    };
    {
      let c = make_address();

      let t = withdraw(~sender=c, ~destination, Amount.of_int(1), t1, t);
      expect.result(t).toBeError();
    };
    ();
  });
  test("compare", (expect, _) => {
    let (t, (t1, _), (a, b)) = setup_two();

    {
      // two tickets with same data
      let t1' = make_ticket(~data=t1.data, ());
      let t = transfer(~sender=a, ~destination=b, Amount.of_int(1), t1', t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      // two tickets with same ticketer
      let t1' = make_ticket(~ticketer=t1.ticketer, ());
      let t = transfer(~sender=a, ~destination=b, Amount.of_int(1), t1', t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    ();
  });
  // TODO: handles stuff
});
