open Deku_tests
open Ledger_tests

let main = Landmark.register "main"

let call_ticket = Landmark.register "make_ticket"
let call_ledger_make_ticket =
  Landmark.wrap call_ticket (fun () -> make_ticket ()) ()

let () =
  let open Landmark in
  enter main;
  ignore call_ledger_make_ticket;
  exit main
