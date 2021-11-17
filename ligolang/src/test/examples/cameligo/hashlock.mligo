(*_*
  name: Hashlock Contract (CameLIGO)
  language: cameligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: |
      Commit (0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce)
    storage: |
      { hashed=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce; 
        unused=false; 
        commits=(Big_map.literal[(
          ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address),  
          {date=("2020-05-29T11:22:33Z" : timestamp); 
           salted_hash=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce}
          );]
        )}
  deploy:
    entrypoint: main
    storage: |
      { hashed=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce; 
        unused=false; 
        commits=(Big_map.literal[(
          ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address),  
          {date=("2020-05-29T11:22:33Z" : timestamp); 
           salted_hash=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce}
          );]
        )}
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: commit
    parameters: |
      ( 0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce, 
        { hashed=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce; 
          unused=false; 
          commits=(Big_map.literal[(
            ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), {date=("2020-05-29T11:22:33Z" : timestamp); salted_hash=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce}
            );]
          )
        }
      )
  generateDeployScript:
    entrypoint: main
    storage: |
      { hashed=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce; 
        unused=false; 
        commits=(Big_map.literal[(
          ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address),  
          {date=("2020-05-29T11:22:33Z" : timestamp); 
           salted_hash=0x0e2ab5866b0ec701a0204881645dc50e1d60668f1433a385e999f0af1b6cd8ce}
          );]
        )}
*_*)
type commit = {
  date        : timestamp;
  salted_hash : bytes;
}

type commit_set = (address, commit) big_map

type storage = {
  hashed  : bytes;
  unused  : bool;
  commits : commit_set
}

type reveal = {
  hashable : bytes;
  message  : unit -> operation list
}

type parameter =
  Commit of bytes
| Reveal of reveal

type return = operation list * storage

(* We use hash-commit so that a baker can not steal *)

let commit (p, s : bytes * storage) : return =
  let commit : commit =
    {date = Tezos.now + 86_400; salted_hash = p} in
  let updated_map: commit_set =
    Big_map.update Tezos.sender (Some commit) s.commits in
  let s = {s with commits = updated_map}
  in ([] : operation list), s

let reveal (p, s : reveal * storage) : return =
  if not s.unused
  then
    (failwith "This contract has already been used." : return)
  else
    let commit : commit =
      match Big_map.find_opt sender s.commits with
    | Some c -> c
    | None ->
       (failwith "You have not made a commitment to hash against yet."
        : commit)
    in
    if Tezos.now < commit.date
    then
      (failwith "It has not been 24 hours since your commit yet.": return)
    else
      let salted =
        Crypto.sha256 (Bytes.concat p.hashable (Bytes.pack sender)) in
      if salted <> commit.salted_hash
      then
        (failwith "This reveal does not match your commitment.": return)
      else
        if s.hashed = Crypto.sha256 p.hashable
        then
          let s : storage = {s with unused = false}
          in p.message (), s
        else (failwith "Your commitment did not match the storage hash."
              : return)

let main (p, s : parameter * storage) : return =
  match p with
  | Commit c -> commit (c,s)
  | Reveal r -> reveal (r,s)
