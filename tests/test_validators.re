open Setup;
open Protocol;
open Validators;

describe("validators", ({test, _}) => {
  let make_validator = () => {
    open Mirage_crypto_ec;
    let (_key, address) = Ed25519.generate();
    Validators.{address: address};
  };
  let setup_two = () => {
    let a = make_validator();
    let b = make_validator();
    let t = empty |> add(a) |> add(b);
    (t, a, b);
  };

  test("empty", ({expect, _}) => {
    let t = empty;
    expect.equal(current(t), None);
    expect.equal(to_list(t), []);
    expect.equal(length(t), 0);
  });
  test("add", ({expect, _}) => {
    let a = make_validator();
    let t = empty |> add(a);
    // TODO: is this desirable? Auto changing current on add
    expect.equal(current(t), Some(a));
    expect.equal(to_list(t), [a]);
    expect.equal(length(t), 1);

    let b = make_validator();
    let t = t |> add(b);
    expect.equal(current(t), Some(a));
    expect.equal(to_list(t), [a, b]);
    expect.equal(length(t), 2);
  });
  test("remove", ({expect, _}) => {
    {
      let (t, a, b) = setup_two();
      let t = t |> remove(b);
      expect.equal(current(t), Some(a));
      expect.equal(to_list(t), [a]);
      expect.equal(length(t), 1);

      let t = t |> remove(a);
      expect.equal(t, empty);
    };
    {
      let (t, a, b) = setup_two();
      let t = t |> remove(a);
      expect.equal(current(t), Some(b));
      expect.equal(to_list(t), [b]);
      expect.equal(length(t), 1);

      let t = t |> remove(b);
      expect.equal(t, empty);
    };
    {
      // remove unknown
      let (t, _, _) = setup_two();
      let unknown = make_validator();
      expect.equal(remove(unknown, t), t);
    };
    ();
  });
  test("after_current", ({expect, _}) => {
    {
      expect.equal(after_current(0, empty), None);
      expect.equal(after_current(1, empty), None);
      expect.equal(after_current(-1, empty), None);
    };
    let (t, a, b) = setup_two();
    let c = make_validator();
    let t = t |> add(c);
    expect.equal(current(t), Some(a));
    expect.equal(to_list(t), [a, b, c]);
    expect.equal(length(t), 3);

    expect.equal(after_current(0, t), Some(a));
    expect.equal(after_current(1, t), Some(b));
    expect.equal(after_current(2, t), Some(c));
    expect.equal(after_current(3, t), Some(a));
    expect.equal(after_current(4, t), Some(b));
    expect.equal(after_current(-1, t), Some(c));
    expect.equal(after_current(-2, t), Some(b));
    expect.equal(after_current(-3, t), Some(a));
    expect.equal(after_current(-4, t), Some(c));
  });
  test("update_current", ({expect, _}) => {
    let (t, a, b) = setup_two();
    expect.equal(current(t), Some(a));

    let t = update_current(b.address, t);
    expect.equal(current(t), Some(b));

    // ensure this is a noop
    expect.equal(update_current(b.address, t), t);

    // ensure it is None when not a validator
    // TODO: is this a good behavior?
    let unknown = make_validator();
    expect.equal(update_current(unknown.address, t) |> current, None);
    ();
  });
  // TODO: hash
});
