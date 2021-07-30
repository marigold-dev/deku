open Tezos_interop;

// TODO: I feel uneasy about this module
module Address = {
  include Address;
  let with_yojson_string = (name, of_string, to_string) =>
    Helpers.with_yojson_string(
      string =>
        of_string(string) |> Option.to_result(~none="invalid " ++ name),
      to_string,
    );
  let (of_yojson, to_yojson) =
    with_yojson_string("address", of_string, to_string);
};

[@deriving yojson]
type t =
  Ticket.t = {
    ticketer: Address.t,
    data: bytes,
  };

// TODO: should we expose to_yojson here?
let compare = (a, b) => {
  let to_string = a =>
    Bytes.to_string(a.data) ++ Tezos_interop.Address.to_string(b.ticketer);
  String.compare(to_string(a), to_string(b));
};
