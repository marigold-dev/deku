open Setup
open Crypto
module M = struct
  type t = {
    key : int;
    hash : BLAKE2B.t;
  }
  [@@deriving yojson]
  let hash t = t.hash
  let make key = { key; hash = BLAKE2B.hash (string_of_int key) }
end
open M
open Incremental_patricia.Make (M)
let () =
  describe "incremental Patricia" (fun { test; _ } ->
      test "add and find" (fun { expect; _ } ->
          let add_and_test tree () =
            let tree, value = add make tree in
            let _, stored_value = find value.key tree |> Option.get in
            expect.equal value stored_value;
            tree in
          let size = 1 in
          let tree =
            List.init size (fun _ -> ()) |> List.fold_left add_and_test empty
          in
          let _ =
            List.init size (fun n -> (expect.option (find n tree)).toBeSome ())
          in
          ());
      test "hash" (fun { expect; _ } ->
          let tree = empty in
          let tree, a = add make tree in
          expect.equal (hash tree) a.hash;
          let tree, b = add make tree in
          expect.equal (hash tree) (BLAKE2B.both a.hash b.hash);
          let tree, c = add make tree in
          expect.equal (hash tree)
            (BLAKE2B.both
               (BLAKE2B.both a.hash b.hash)
               (BLAKE2B.both c.hash (hash empty)));
          let tree, d = add make tree in
          expect.equal (hash tree)
            (BLAKE2B.both
               (BLAKE2B.both a.hash b.hash)
               (BLAKE2B.both c.hash d.hash))))
