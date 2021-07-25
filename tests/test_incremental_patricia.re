open Setup;
open Helpers;

module M = {
  [@deriving yojson]
  type t = {
    key: int,
    hash: BLAKE2B.t,
  };
  let hash = t => t.hash;
  let make = key => {key, hash: BLAKE2B.hash(string_of_int(key))};
};
open M;
open Incremental_patricia.Make(M);

describe("incremental Patricia", ({test, _}) => {
  test("add and find", ({expect, _}) => {
    let add_and_test = (tree, ()) => {
      let (tree, value) = add(make, tree);
      let (_, stored_value) = find(value.key, tree) |> Option.get;
      expect.equal(value, stored_value);
      tree;
    };
    let size = 1;
    let tree =
      List.init(size, _ => ()) |> List.fold_left(add_and_test, empty);
    let _ = List.init(size, n => expect.option(find(n, tree)).toBeSome());
    ();
  });
  test("hash", ({expect, _}) => {
    let tree = empty;
    let (tree, a) = add(make, tree);

    expect.equal(hash(tree), a.hash);
    let (tree, b) = add(make, tree);
    expect.equal(hash(tree), BLAKE2B.both(a.hash, b.hash));

    let (tree, c) = add(make, tree);
    expect.equal(
      hash(tree),
      BLAKE2B.both(
        BLAKE2B.both(a.hash, b.hash),
        BLAKE2B.both(c.hash, hash(empty)),
      ),
    );

    let (tree, d) = add(make, tree);
    expect.equal(
      hash(tree),
      BLAKE2B.both(
        BLAKE2B.both(a.hash, b.hash),
        BLAKE2B.both(c.hash, d.hash),
      ),
    );
  });
});
