open Setup
open Protocol
open Validators
let () =
  describe "validators" (fun { test; _ } ->
      let make_validator () =
        let open Crypto in
        let _key, wallet = Ed25519.generate () in
        let address = Key_hash.of_key (Ed25519 wallet) in
        let open Validators in
        { address } in
      let setup_two () =
        let a = make_validator () in
        let b = make_validator () in
        let t = empty |> add a |> add b in
        (t, a, b) in
      test "empty" (fun { expect; _ } ->
          let t = empty in
          expect.equal (current t) None;
          expect.equal (to_list t) [];
          expect.equal (length t) 0);
      test "add" (fun { expect; _ } ->
          let a = make_validator () in
          let t = empty |> add a in
          expect.equal (current t) (Some a);
          expect.equal (to_list t) [a];
          expect.equal (length t) 1;
          let b = make_validator () in
          let t = t |> add b in
          expect.equal (current t) (Some a);
          expect.equal (to_list t) [a; b];
          expect.equal (length t) 2);
      test "remove" (fun { expect; _ } ->
          (let t, a, b = setup_two () in
           let t = t |> remove b in
           expect.equal (current t) (Some a);
           expect.equal (to_list t) [a];
           expect.equal (length t) 1;
           let t = t |> remove a in
           expect.equal t empty);
          (let t, a, b = setup_two () in
           let t = t |> remove a in
           expect.equal (current t) (Some b);
           expect.equal (to_list t) [b];
           expect.equal (length t) 1;
           let t = t |> remove b in
           expect.equal t empty);
          (let t, _, _ = setup_two () in
           let unknown = make_validator () in
           expect.equal (remove unknown t) t);
          ());
      test "after_current" (fun { expect; _ } ->
          expect.equal (after_current 0 empty) None;
          expect.equal (after_current 1 empty) None;
          expect.equal (after_current (-1) empty) None;
          let t, a, b = setup_two () in
          let c = make_validator () in
          let t = t |> add c in
          expect.equal (current t) (Some a);
          expect.equal (to_list t) [a; b; c];
          expect.equal (length t) 3;
          expect.equal (after_current 0 t) (Some a);
          expect.equal (after_current 1 t) (Some b);
          expect.equal (after_current 2 t) (Some c);
          expect.equal (after_current 3 t) (Some a);
          expect.equal (after_current 4 t) (Some b);
          expect.equal (after_current (-1) t) (Some c);
          expect.equal (after_current (-2) t) (Some b);
          expect.equal (after_current (-3) t) (Some a);
          expect.equal (after_current (-4) t) (Some c));
      test "update_current" (fun { expect; _ } ->
          let t, a, b = setup_two () in
          expect.equal (current t) (Some a);
          let t = update_current b.address t in
          expect.equal (current t) (Some b);
          expect.equal (update_current b.address t) t;
          let unknown = make_validator () in
          expect.equal (update_current unknown.address t |> current) None;
          ()))
