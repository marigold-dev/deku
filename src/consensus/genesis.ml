open Deku_concepts
open Deku_crypto

let identity =
  let b58 = "edsk2icTz8hTkajZKfqcH3V5aJnBd5AHavJLGmzi8mQo16zW4Z36zS" in
  match Secret.of_b58 b58 with
  | Some secret -> Identity.make secret
  | None -> failwith "bug when parsing edsk key"

let block =
  let level = Level.zero in
  let previous = Block_hash.hash "tuturu" in
  let operations = [] in
  Block.produce ~identity ~level ~previous ~operations
