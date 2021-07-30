open Setup;
open Helpers;
open Protocol;
open Ledger;

describe("ledger", ({test, _}) => {
  // TODO: maybe have a "total amount" function to ensure invariant?
  let make_ticket = () => {
    open Tezos_interop;
    let random_hash =
      Mirage_crypto_rng.generate(20)
      |> Cstruct.to_string
      |> BLAKE2B_20.of_raw_string
      |> Option.get;
    let ticketer = Address.Originated(random_hash);
    let data = Mirage_crypto_rng.generate(256) |> Cstruct.to_bytes;
    Ticket.{ticketer, data};
  };
  let _ = make_ticket();
  let make_wallet = () => {
    open Mirage_crypto_ec;
    let (_key, address) = Ed25519.generate();
    Wallet.of_address(address);
  };
  let setup_two = () => {
    let t1 = make_ticket();
    let t2 = make_ticket();

    let a = make_wallet();
    let b = make_wallet();
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
  test("balance", (_, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();
    expect_balance(a, t1, 100, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 200, t);
    expect_balance(b, t2, 400, t);

    expect_balance(make_wallet(), t1, 0, t);
    expect_balance(make_wallet(), t2, 0, t);
    expect_balance(a, make_ticket(), 0, t);
    expect_balance(b, make_ticket(), 0, t);
  });
  test("transfer", (expect, expect_balance) => {
    let (t, (t1, t2), (a, b)) = setup_two();
    let c = make_wallet();

    let t = transfer(~source=a, ~destination=b, Amount.of_int(1), t1, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 300, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 400, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 0, t);

    let t = transfer(~source=b, ~destination=a, Amount.of_int(3), t2, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 397, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 0, t);

    let t = transfer(~source=b, ~destination=c, Amount.of_int(5), t2, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 99, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 392, t);
    expect_balance(c, t1, 0, t);
    expect_balance(c, t2, 5, t);

    let t = transfer(~source=a, ~destination=c, Amount.of_int(99), t1, t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect_balance(a, t1, 0, t);
    expect_balance(a, t2, 303, t);
    expect_balance(b, t1, 201, t);
    expect_balance(b, t2, 392, t);
    expect_balance(c, t1, 99, t);
    expect_balance(c, t2, 5, t);

    {
      let t = transfer(~source=b, ~destination=c, Amount.of_int(202), t1, t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      let d = make_wallet();
      let t = transfer(~source=d, ~destination=c, Amount.of_int(1), t2, t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      let t3 = make_ticket();
      let t = transfer(~source=a, ~destination=b, Amount.of_int(1), t3, t);
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
});
