type id = int

type id_details = {
  owner: address;
  controller: address;
  profile: bytes
}

type buy = {
  profile: bytes;
  initial_controller: address option;
}

type update_owner = {
  id: id;
  new_owner: address;
}

type update_details = {
  id: id;
  new_profile: bytes option;
  new_controller: address option;
}

type action =
| Buy of buy
| Update_owner of update_owner
| Update_details of update_details
| Skip

(* The prices kept in storage can be changed by bakers, though they
   should only be adjusted down over time, not up. *)

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage = {
  identities: (id, id_details) big_map;
  next_id: int;
  name_price: tez;
  skip_price: tez;
}

type return = operation list * storage

(* Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.  5 three
letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw
digits. This can be stored as a single integer which is then
translated into the corresponding series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does
need to cost something so people don't eat up the address space. 256^5
means you have a lot of address space, but if people troll by skipping
a lot that could be eaten up.  Should probably do some napkin
calculations for how expensive skipping needs to be to deter people
from doing it just to chew up address space.  *)

let buy (parameter, storage: buy * storage) =
  let () : unit =
    if amount = storage.name_price
    then ()
    else (failwith "Incorrect amount paid.": unit)
  in
  let profile = parameter.profile in
  let initial_controller = parameter.initial_controller in
  let identities = storage.identities in
  let new_id = storage.next_id in
  let controller: address =
    match initial_controller with
    | Some addr -> addr
    | None -> sender in
  let new_id_details: id_details = {
    owner = sender;
    controller = controller;
    profile = profile} in
  let updated_identities : (id, id_details) big_map =
    Big_map.update new_id (Some new_id_details) identities
  in
  ([]: operation list), {storage with identities = updated_identities;
                         next_id = new_id + 1;
                        }

let update_owner (parameter, storage: update_owner * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating owner doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_owner = parameter.new_owner in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let () : unit =
    if sender = current_id_details.owner
    then ()
    else failwith "You are not the owner of this ID."
  in
  let updated_id_details: id_details = {
    owner = new_owner;
    controller = current_id_details.controller;
    profile = current_id_details.profile;
  }
  in
  let updated_identities = Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {storage with identities = updated_identities}

let update_details (parameter, storage: update_details * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating details doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_profile = parameter.new_profile in
  let new_controller = parameter.new_controller in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let () : unit =
    if (sender = current_id_details.controller) || (sender = current_id_details.owner)
    then ()
    else failwith ("You are not the owner or controller of this ID.")
  in
  let owner: address = current_id_details.owner in
  let profile: bytes =
    match new_profile with
    | None -> (* Default *) current_id_details.profile
    | Some new_profile -> new_profile
  in
  let controller: address =
    match new_controller with
    | None -> (* Default *) current_id_details.controller
    | Some new_controller -> new_controller
  in
  let updated_id_details: id_details = {
    owner = owner;
    controller = controller;
    profile = profile;
  }
  in
  let updated_identities: (id, id_details) big_map  =
    Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {storage with identities = updated_identities}

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
let skip (_,storage: unit * storage) =
  let () : unit =
    if amount = storage.skip_price
    then ()
    else failwith "Incorrect amount paid."
  in
  ([]: operation list), {storage with next_id = storage.next_id + 1}

let main (action, storage : action * storage) : return =
  match action with
  | Buy b -> buy (b, storage)
  | Update_owner uo -> update_owner (uo, storage)
  | Update_details ud -> update_details (ud, storage)
  | Skip -> skip ((), storage)
