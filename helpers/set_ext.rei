include (module type of Set);
module Make_with_yojson:
  (
    K: {
      type t;
      let compare: (t, t) => int;
      let to_yojson: t => Yojson.Safe.t;
      let of_yojson: Yojson.Safe.t => result(t, string);
    },
  ) =>
   {
    include (module type of Set.Make(K));
    let to_yojson: t => Yojson.Safe.t;

    let of_yojson: Yojson.Safe.t => result(t, string);
  };
