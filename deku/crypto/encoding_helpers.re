let unexpected_data = (~name) =>
  Format.kasprintf(invalid_arg, "Unexpected data (%s)", name);

let make_encoding = (~name, ~title, ~to_string, ~of_string, ~raw_encoding) => {
  open Data_encoding;
  let of_string_exn = string =>
    switch (of_string(string)) {
    | Some(t) => t
    | None => unexpected_data(~name)
    };

  let json_encoding =
    conv(to_string, Json.wrap_error(of_string_exn), string);

  splitted(
    ~binary=obj1(req(name, raw_encoding)),
    ~json=def(name, ~title=title ++ " (Base58Check-encoded)", json_encoding),
  );
};

let rec parse_string_variant = (l, string) =>
  switch (l) {
  | [of_string, ...l] =>
    switch (of_string(string)) {
    | Some(v) => Some(v)
    | None => parse_string_variant(l, string)
    }
  | [] => None
  };

module Make_b58 =
       (
         H: {
           type t;
           let name: string;
           let title: string;

           let prefix: string;
           let size: int;

           let to_raw: t => string;
           let of_raw: string => option(t);
         },
       ) => {
  open H;

  // b58 string
  let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
  let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);

  // pack encoding
  let of_raw_exn = string =>
    switch (of_raw(string)) {
    | Some(t) => t
    | None => unexpected_data(~name)
    };
  let encoding =
    make_encoding(
      ~name,
      ~title,
      ~to_string,
      ~of_string,
      ~raw_encoding=
        Data_encoding.(conv(to_raw, of_raw_exn, Fixed.string(size))),
    );
};
