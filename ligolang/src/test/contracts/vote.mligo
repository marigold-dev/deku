type storage = {
  title       : string;
  yea         : nat;
  nay         : nat;
  voters      : address set;
  start_time  : timestamp;
  finish_time : timestamp
}

type return = operation list * storage

type vote = Yea | Nay

type reset = {
  title       : string;
  start_time  : timestamp;
  finish_time : timestamp
}

type parameter =
| Vote  of vote
| Reset of reset

let reset (reset, _ : reset * storage) : return =
  ([] : operation list),
  {title       = reset.title;
   yea         = 0n;
   nay         = 0n;
   voters      = (Set.empty : address set);
   start_time  = reset.start_time;
   finish_time = reset.finish_time}

let vote (vote, store : vote * storage) : return =
  (* let now = Tezos.now in
     let _ = assert (now >= store.start_time && store.finish_time > now) in *)
  let addr = Tezos.sender in
  (* let _ = assert (not Set.mem addr store.voters) in *)
  let store =
    match vote with
      Yea -> {store with yea = store.yea + 1n}
    | Nay -> {store with nay = store.nay + 1n}
  in ([] : operation list),
     {store with voters = Set.add addr store.voters}

let main (action, store : parameter * storage) : return =
  match action with
  | Vote v  -> vote (v, store)
  | Reset r -> reset (r, store)
