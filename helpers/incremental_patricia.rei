module Make:
  (
    V: {
      [@deriving yojson]
      type t;
      let hash: t => BLAKE2B.t;
    },
  ) =>
   {
    type value = V.t;
    type key = int;
    [@deriving yojson]
    type t;

    let empty: t;
    let hash: t => BLAKE2B.t;
    let add: (key => value, t) => (t, value);
    let find: (key, t) => option((list((BLAKE2B.t, BLAKE2B.t)), value));
  };
