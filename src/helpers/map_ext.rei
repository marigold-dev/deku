include (module type of Map);

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
    include (module type of Map.Make(K));
    let to_yojson: ('a => Yojson.Safe.t, t('a)) => Yojson.Safe.t;
    let of_yojson:
      (Yojson.Safe.t => result('a, string), Yojson.Safe.t) =>
      result(t('a), string);
  };
