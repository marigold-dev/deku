open Tezos_micheline;
open Helpers;

[@deriving (eq, ord)]
type t = {
  ticketer: Address.t,
  data: bytes,
};

let parse_micheline = string => {
  let (tokens, errors) = Micheline_parser.tokenize(string);
  switch (errors) {
  | [] =>
    let (micheline, errors) = Micheline_parser.parse_expression(tokens);
    switch (errors) {
    | [] => Some(micheline)
    | _ => None
    };
  | _ => None
  };
};

let to_string = t => {
  let loc = Micheline_printer.{comment: None};
  let micheline =
    Micheline.Prim(
      loc,
      "Pair",
      [String(loc, Address.to_string(t.ticketer)), Bytes(loc, t.data)],
      [],
    );
  Format.asprintf("%a", Micheline_printer.print_expr, micheline);
};
let of_string = string => {
  let.some micheline = parse_micheline(string);
  let.some (ticketer, data) =
    switch (micheline) {
    // TODO: maybe full Michelson_v1_parser
    | Prim(_, "Pair", [String(_, ticketer), Bytes(_, data)], []) =>
      Some((ticketer, data))
    | _ => None
    };
  let.some ticketer = Address.of_string(ticketer);
  Some({ticketer, data});
};

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("ticket_id", to_string, of_string);
