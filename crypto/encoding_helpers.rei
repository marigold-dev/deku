let make_encoding:
  (
    ~name: string,
    ~title: string,
    ~to_string: 'a => string,
    ~of_string: string => option('a),
    ~raw_encoding: Data_encoding.t('a)
  ) =>
  Data_encoding.t('a);

/** [parse_string_variant of_string_list string] try to parse the string
    using every of_string in the list, returns [Some 'a] when one matches */
let parse_string_variant:
  (list(string => option('a)), string) => option('a);

module Make_b58:
  (
    H: {
      type t;
      // data encoding metadata
      let name: string;
      let title: string;

      // b58
      let prefix: string;
      let size: int;

      let to_raw: t => string;
      let of_raw: string => option(t);
    },
  ) =>
   {
    open H;

    /** [to_string t] encodeds t as a b58 string */
    let to_string: t => string;
    /** [of_string string] is [Some t] if string contains a b58 of t */
    let of_string: string => option(t);

    /** [encoding] is identical to Tezos PACK encoding */
    let encoding: Data_encoding.t(t);
  };
