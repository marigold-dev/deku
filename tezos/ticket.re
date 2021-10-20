open Helpers;
open Tezos_micheline;

[@deriving eq]
type t = {
  // TODO: should we allow implicit contracts here?
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
