type t = {
  identity : Consensus.identity;
  minimum_block_delay : float;
}
[@@deriving yojson]

let make ~identity ~minimum_block_delay =
  if minimum_block_delay < 0. then
    failwith "Minimum block delay must be positive";
  { identity; minimum_block_delay }
