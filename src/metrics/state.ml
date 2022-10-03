open Prometheus

let commits_to_tezos =
  let help = "Number of state commits to tezos" in
  Counter.v ~help "deku_commits_to_tezos"

let commit_state () = Counter.inc_one commits_to_tezos