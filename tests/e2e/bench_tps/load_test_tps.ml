open Cmdliner
open Load_test_helpers
open Helpers

(*let params = [(1000, 1); (500, 2); (250, 4); (125, 8); (100, 10)]*)

let params = [(1000, 1); (500, 2); (250, 4); (125, 8); (100, 10)]

let spams_params ~ticketer =
  params
  |> List.map (fun (n, rounds) ->
         Format.eprintf "(n:%i, rounds:%i)\n%!" n rounds;
         spam ~ticketer ~n ~rounds)

let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  Format.eprintf "Starting block level: %Li\n%!" starting_block_level;
  let sps = spams_params ~ticketer in
  let time = 20.0 in
  Format.eprintf "Run each spam for %.03f seconds\n" time;
  Lwt_list.iter_p (fun s -> Lwt.pick [Lwt_unix.timeout time; s]) sps

(*let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level ;
  Lwt.pick
    [ Lwt_unix.timeout 10.0
    ; spam ~ticketer ~n:1000 ~rounds:1
    ; spam ~ticketer ~n:500 ~rounds:2 ]*)

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-tps") args
