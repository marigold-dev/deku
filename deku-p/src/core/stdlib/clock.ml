let last_sleep : float ref = ref 0.0

let sleep env duration =
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let sleep = duration -. (now -. !last_sleep) in
  if sleep > 0. then Eio.Time.sleep clock sleep;
  last_sleep := Eio.Time.now clock
