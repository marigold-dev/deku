/*
(*_*
  name: Hashlock Contract (ReasonLIGO)
  language: reasonligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: |
      Commit (0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce)
    storage: |
      {
        hashed:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
        unused:false, 
        commits:Big_map.literal([(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), {date:("2020-06-02T10:23:41Z":timestamp), salted_hash:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce})])
      }
  deploy:
    entrypoint: main
    storage: |
      {
        hashed:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
        unused:false, 
        commits:Big_map.literal([(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), {date:("2020-06-02T10:23:41Z":timestamp), salted_hash:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce})])
      }
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: commit
    parameters: |
      (
        0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
        {
          hashed:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
          unused:false, 
          commits:Big_map.literal([(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), {date:("2020-06-02T10:23:41Z":timestamp), salted_hash:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce})])
        }
      )
  generateDeployScript:
    entrypoint: main
    storage: |
      {
        hashed:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
        unused:false, 
        commits:Big_map.literal([(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), {date:("2020-06-02T10:23:41Z":timestamp), salted_hash:0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce})])
      }
*_*)
*/
type commit = {
  date:        timestamp,
  salted_hash: bytes,
}

type commit_set = big_map(address, commit)

type storage = {
  hashed: bytes,
  unused: bool,
  commits: commit_set
}

type reveal = {
  hashable: bytes,
  message: (unit => list(operation))
}

type parameter =
  Commit (bytes)
| Reveal (reveal);

type return = (list(operation), storage)

/* We use hash-commit so that a baker can not steal */

let commit = ((p, s) : (bytes, storage)) : return => {
  let commit : commit = {date: Tezos.now + 86_400, salted_hash: p};
  let updated_map: commit_set = Big_map.update(Tezos.sender, Some(commit), s.commits);
  let s = {...s, commits: updated_map};
  (([] : list(operation)), s);
};

let reveal = ((p, s): (reveal, storage)) : return => {
  if (!s.unused) {
    failwith("This contract has already been used.");
  }
  else { (); };
  let commit_ : commit =
    switch (Big_map.find_opt(sender, s.commits)) {
    | Some (c) => c
    | None =>
       (failwith("You have not made a commitment to hash against yet."): commit)
    };
  if (Tezos.now < commit_.date) {
    failwith("It has not been 24 hours since your commit yet.");
  }
  else { (); };
  let salted : bytes =
    Crypto.sha256(
      Bytes.concat(p.hashable, Bytes.pack(Tezos.sender))
    );
  if (salted != commit_.salted_hash) {
    failwith("This reveal does not match your commitment.");
  }
  else { (); };
  if (s.hashed == Crypto.sha256(p.hashable)) {
    let s : storage = {...s, unused: false};
    (p.message(), s)
  }
  else {
    (failwith("Your commitment did not match the storage hash.") : return);
  };
};

let main = ((p, s): (parameter, storage)) : return => {
  switch (p) {
  | Commit (c) => commit((c,s))
  | Reveal (r) => reveal((r,s))
  };
};
