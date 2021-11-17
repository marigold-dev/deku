type id = int

type id_details = {
  owner: address,
  controller: address,
  profile: bytes,
}

type buy = {
  profile: bytes,
  initial_controller: option(address),
}

type update_owner = {
  id: id,
  new_owner: address,
}

type update_details = {
  id: id,
  new_profile: option(bytes),
  new_controller: option(address),
}

type action =
| Buy(buy)
| Update_owner(update_owner)
| Update_details(update_details)
| Skip(unit)

/* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. */
type storage = {
  identities: big_map (id, id_details),
  next_id: int,
  name_price: tez,
  skip_price: tez,
}

/** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*/

let buy = ((parameter, storage): (buy, storage)) : (list(operation), storage) => {
  let _ : unit =
    if (amount == storage.name_price) { (); }
    else { failwith("Incorrect amount paid."); };
  let profile = parameter.profile;
  let initial_controller = parameter.initial_controller;
  let identities = storage.identities;
  let new_id = storage.next_id;
  let controller: address =
    switch (initial_controller) {
      | Some(addr) => addr
      | None => sender
    };
  let new_id_details: id_details = {
    owner : sender,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(new_id, Some(new_id_details), identities);
  (([]: list(operation)), { ...storage,
                           identities : updated_identities,
                           next_id : new_id + 1,
                        });
  };

let update_owner = ((parameter, storage): (update_owner, storage)) : (list(operation), storage) => {
  let _ : unit =
    if (amount != 0mutez) {
      failwith("Updating owner doesn't cost anything.");
    }
    else { (); };
  let id : int = parameter.id;
  let new_owner = parameter.new_owner;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let _ : unit =
    if (sender == current_id_details.owner) { (); }
    else { failwith("You are not the owner of this ID."); };
  let updated_id_details: id_details = {
    owner : new_owner,
    controller : current_id_details.controller,
    profile : current_id_details.profile,
  };
  let updated_identities = Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), { ...storage, identities : updated_identities });
  };

let update_details = ((parameter, storage): (update_details, storage)) :
                   (list(operation), storage) => {
  let _ : unit =
    if (amount != 0mutez) {
      failwith("Updating details doesn't cost anything.");
    }
    else { (); };
  let id = parameter.id;
  let new_profile = parameter.new_profile;
  let new_controller = parameter.new_controller;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let _ : unit =
    if ((sender != current_id_details.controller) &&
        (sender != current_id_details.owner)) {
      failwith ("You are not the owner or controller of this ID.")
    }
    else { (); };
  let owner: address = current_id_details.owner;
  let profile: bytes =
    switch (new_profile) {
      | None => /* Default */ current_id_details.profile
      | Some(new_profile) => new_profile
    };
  let controller: address =
    switch (new_controller) {
      | None => /* Default */ current_id_details.controller
      | Some new_controller => new_controller
    };
  let updated_id_details: id_details = {
    owner : owner,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), { ...storage, identities : updated_identities });
  };

/* Let someone skip the next identity so nobody has to take one that's undesirable */
let skip = ((_,storage): (unit, storage)) => {
  let _ : unit =
    if (amount != storage.skip_price) {
      failwith("Incorrect amount paid.");
    }
    else { (); };
  (([]: list(operation)), { ...storage, next_id : storage.next_id + 1 });
  };

let main = ((action, storage): (action, storage)) : (list(operation), storage) => {
  switch (action) {
    | Buy(b) => buy((b, storage))
    | Update_owner(uo) => update_owner((uo, storage))
    | Update_details ud => update_details((ud, storage))
    | Skip _ => skip(((), storage))
  };
};
