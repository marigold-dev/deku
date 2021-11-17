type id is int

type id_details is
  record [
    owner: address;
    controller: address;
    profile: bytes;
  ]

type buy is
  record [
    profile: bytes;
    initial_controller: option(address);
  ]

type update_owner is
  record [
    id: id;
    new_owner: address;
  ]

type update_details is
  record [
    id: id;
    new_profile: option(bytes);
    new_controller: option(address);
  ]

type action is
  | Buy of buy
  | Update_owner of update_owner
  | Update_details of update_details
  | Skip of unit

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage is
  record [
    identities: big_map (id, id_details);
    next_id: int;
    name_price: tez;
    skip_price: tez;
  ]

(** Preliminary thoughts on ids:

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
*)

function buy (const parameter : buy; const storage : storage) : list(operation) * storage is
  begin
    if amount = storage.name_price
    then skip
    else failwith("Incorrect amount paid.");
    const profile : bytes = parameter.profile;
    const initial_controller : option(address) = parameter.initial_controller;
    var identities : big_map (id, id_details) := storage.identities;
    const new_id : int = storage.next_id;
    const controller : address =
      case initial_controller of
        Some(addr) -> addr
      | None -> sender
      end;
    const new_id_details: id_details =
      record [
              owner = sender ;
              controller = controller ;
              profile = profile ;
      ];
    identities[new_id] := new_id_details;
  end with ((nil : list(operation)), storage with record [
                              identities = identities;
                              next_id = new_id + 1;
                              ])

function update_owner (const parameter : update_owner; const storage : storage) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then
      begin
        failwith("Updating owner doesn't cost anything.");
      end
    else skip;
    const id : int = parameter.id;
    const new_owner : address = parameter.new_owner;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details : id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    if sender = id_details.owner
    then skip;
    else failwith("You are not the owner of this ID.");
    id_details.owner := new_owner;
    identities[id] := id_details;
  end with ((nil: list(operation)), storage with record [ identities = identities; ])

function update_details (const parameter : update_details; const storage : storage ) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then failwith("Updating details doesn't cost anything.")
    else skip;
    const id : int = parameter.id;
    const new_profile : option(bytes) = parameter.new_profile;
    const new_controller : option(address) = parameter.new_controller;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details: id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    if (sender = id_details.controller) or (sender = id_details.owner)
    then skip;
    else failwith("You are not the owner or controller of this ID.");
    const owner: address = id_details.owner;
    const profile: bytes =
      case new_profile of
        None -> (* Default *) id_details.profile
      | Some(new_profile) -> new_profile
      end;
    const controller: address =
    case new_controller of
      None -> (* Default *) id_details.controller
    | Some(new_controller) -> new_controller
    end;
    id_details.owner := owner;
    id_details.controller := controller;
    id_details.profile := profile;
    identities[id] := id_details;
  end with ((nil: list(operation)), storage with record [ identities = identities; ])

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
function skip_ (const _ : unit; const storage: storage) : list(operation) * storage is
  begin
    if amount = storage.skip_price
    then skip
    else failwith("Incorrect amount paid.");
  end with ((nil: list(operation)), storage with record [ next_id = storage.next_id + 1; ])

function main (const action : action; const storage : storage) : list(operation) * storage is
  case action of
  | Buy(b) -> buy (b, storage)
  | Update_owner(uo) -> update_owner (uo, storage)
  | Update_details(ud) -> update_details (ud, storage)
  | Skip(_) -> skip_ (unit, storage)
  end;
