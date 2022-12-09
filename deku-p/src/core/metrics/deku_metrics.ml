(* FIXME: there are better ways to do this when we split the api from the consensus node (PR #879)*)

let rec take n l =
  match (n, l) with
  | 0, _ -> []
  | _, [] -> []
  | n, hd :: tl -> hd :: take (n - 1) tl

type stats = { latency : float; tps : float }

let latest_tx_count = ref []

let set_latest_tx_count tx_count =
  let updated = (Unix.gettimeofday (), tx_count) :: !latest_tx_count in
  latest_tx_count := take 10 updated

let get_statistics () =
  let transactions =
    !latest_tx_count |> List.map snd |> List.fold_left ( + ) 0
  in
  match !latest_tx_count with
  | [] | [ _ ] -> { latency = 0.; tps = 0. }
  | l ->
      let hd, _ = List.hd l in
      let tl, _ = List.rev l |> List.hd in
      let total_delta = hd -. tl in
      let total_blocks = List.length !latest_tx_count in
      let latency = total_delta /. Float.of_int total_blocks in
      let tps = Float.of_int transactions /. total_delta in
      { latency; tps }
