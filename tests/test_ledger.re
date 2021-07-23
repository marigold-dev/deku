open Setup;
open Protocol;
open Ledger;

describe("ledger", ({test, _}) => {
  // TODO: maybe have a "total amount" function to ensure invariant?
  let make_wallet = () => {
    open Mirage_crypto_ec;
    let (_key, address) = Ed25519.generate();
    Wallet.of_address(address);
  };
  let setup_two = () => {
    let a = make_wallet();
    let b = make_wallet();
    let t =
      empty
      |> deposit(a, Amount.of_int(100))
      |> deposit(b, Amount.of_int(200));
    (t, a, b);
  };
  test("balance", ({expect, _}) => {
    let (t, a, b) = setup_two();
    expect.equal(balance(a, t), Amount.of_int(100));
    expect.equal(balance(b, t), Amount.of_int(200));

    expect.equal(balance(make_wallet(), t), Amount.zero);
  });
  test("transfer", ({expect, _}) => {
    let (t, a, b) = setup_two();
    let c = make_wallet();

    let t = transfer(~source=a, ~destination=b, Amount.of_int(1), t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect.equal(balance(a, t), Amount.of_int(99));
    expect.equal(balance(b, t), Amount.of_int(201));
    expect.equal(balance(c, t), Amount.of_int(0));

    let t = transfer(~source=b, ~destination=a, Amount.of_int(3), t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect.equal(balance(a, t), Amount.of_int(102));
    expect.equal(balance(b, t), Amount.of_int(198));
    expect.equal(balance(c, t), Amount.of_int(0));

    let t = transfer(~source=b, ~destination=c, Amount.of_int(5), t);
    expect.result(t).toBeOk();
    let t = Result.get_ok(t);
    expect.equal(balance(a, t), Amount.of_int(102));
    expect.equal(balance(b, t), Amount.of_int(193));
    expect.equal(balance(c, t), Amount.of_int(5));

    {
      let t = transfer(~source=b, ~destination=c, Amount.of_int(194), t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    {
      let d = make_wallet();
      let t = transfer(~source=d, ~destination=c, Amount.of_int(1), t);
      expect.result(t).toBeError();
      expect.equal(Result.get_error(t), `Not_enough_funds);
    };
    ();
  });
  test("deposit", ({expect, _}) => {
    let (t, a, b) = setup_two();

    expect.equal(balance(a, t), Amount.of_int(100));
    expect.equal(balance(b, t), Amount.of_int(200));

    let t = deposit(a, Amount.of_int(123), t);
    expect.equal(balance(a, t), Amount.of_int(223));
    expect.equal(balance(b, t), Amount.of_int(200));

    let t = deposit(b, Amount.of_int(456), t);
    expect.equal(balance(a, t), Amount.of_int(223));
    expect.equal(balance(b, t), Amount.of_int(656));
  });
});
