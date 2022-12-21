open Deku_stdlib
open Deku_crypto
open Deku_consensus
open Deku_concepts

let block_testable = Alcotest.testable Block.pp Block.equal

let identity =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let block ?(above = Genesis.block) ~default_block_size () =
  let withdrawal_handles_hash =
    let randn = Stdlib.Random.int 230 in
    Deku_crypto.BLAKE2b.hash (Int.to_string randn)
  in
  let producer = Producer.empty in
  Producer.produce ~identity ~default_block_size ~above ~withdrawal_handles_hash
    producer

let get_from n l =
  let rec find index prev =
    match l with
    | hd :: tl -> (
        match index = 0 with
        | true -> (hd, prev @ tl)
        | false -> find (index - 1) (hd :: prev))
    | [] -> failwith "index out of range"
  in
  find n []

let blocks count =
  let i = List.init count Fun.id in
  List.fold_left
    (fun (above, blocks) i ->
      let b = block ~above ~default_block_size:0 () in
      (b, (i, b) :: blocks))
    (Genesis.block, []) i

let writer_reader (index, (Block { hash; _ } as block : Block.t)) block_storage
    =
  let written = Atomic.make false in
  let writer () =
    Atomic.set written true;
    Deku_block_storage.Block_storage.save_block ~block block_storage
  in
  let reader () =
    let retrieved_block =
      match Atomic.get written with
      | true -> 
            Deku_block_storage.Block_storage.find_block_by_hash ~block_hash:hash
              block_storage
      | false -> None
    in
    let expected =
      match Atomic.get written with true -> Some block | false -> None
    in
    Alcotest.(check' (option block_testable))
      ~msg:
        (Format.sprintf "block %d was written and read successfully in parallel"
           index)
      ~expected ~actual:retrieved_block
  in
  [ writer; reader ]

let build_random_list count block_storage =
  let _, blocks = blocks count in
  let thunks =
    List.map (fun block -> writer_reader block block_storage) blocks
    |> List.concat
  in

  let rec build_random_list len out l =
    let r = Stdlib.Random.int len in
    let elt, rest = get_from r l in
    build_random_list (len - 1) (elt :: out) rest
  in

  build_random_list count [] thunks
