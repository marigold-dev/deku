module Make =
       (
         V: {
           [@deriving yojson]
           type t;
           let hash: t => BLAKE2B.t;
         },
       ) => {
  [@deriving yojson]
  type value = V.t;
  [@deriving yojson]
  type key = int;
  [@deriving yojson]
  type tree =
    | Empty
    | Leaf({
        value,
        hash: BLAKE2B.t,
      })
    | Node({
        left: tree,
        hash: BLAKE2B.t,
        right: tree,
      });
  [@deriving yojson]
  type t = {
    tree,
    top_bit: key,
    last_key: key,
  };

  let is_set = (bit, number) => 1 lsl bit land number != 0;

  let empty_hash = BLAKE2B.hash("");
  let hash_of_t =
    fun
    | Empty => empty_hash
    | Leaf({hash, _})
    | Node({hash, _}) => hash;
  let rec find = (bit, proofs, key, t) =>
    switch (t) {
    | Empty => None
    | Leaf({value, _}) => Some((List.rev(proofs), value))
    | Node({left, right, _}) =>
      let t = is_set(bit, key) ? right : left;
      find(
        bit - 1,
        [(hash_of_t(left), hash_of_t(right)), ...proofs],
        key,
        t,
      );
    };

  let find = (key, t) =>
    if (key >= 0 && key <= t.last_key) {
      find(t.top_bit - 1, [], key, t.tree);
    } else {
      None;
    };

  let node = (left, right) => {
    let hash = BLAKE2B.both(hash_of_t(left), hash_of_t(right));
    Node({left, hash, right});
  };

  let rec empty = n =>
    if (n == 0) {
      Empty;
    } else {
      let tree = empty(n - 1);
      node(tree, tree);
    };

  // TODO: maybe add should also return the full proof?
  let add = (f, t) => {
    let rec add = (bit, key, value, t) =>
      switch (bit, t) {
      | ((-1), Empty) => Leaf({value, hash: V.hash(value)})
      | (_, Node({left, right, _})) =>
        is_set(bit, key)
          ? node(left, add(bit - 1, key, value, right))
          : node(add(bit - 1, key, value, left), right)
      | _ => assert(false)
      };

    let key = t.last_key + 1;
    let value = f(key);
    let increase_top_bit = key lsr t.top_bit;
    let top_bit = t.top_bit + increase_top_bit;
    let tree =
      if (increase_top_bit == 1) {
        let right = empty(top_bit - 1);
        node(t.tree, right);
      } else {
        t.tree;
      };

    let tree = add(top_bit - 1, key, value, tree);
    ({tree, top_bit, last_key: key}, value);
  };
  let empty = {top_bit: 0, tree: Empty, last_key: (-1)};
  let hash = t => hash_of_t(t.tree);
};
