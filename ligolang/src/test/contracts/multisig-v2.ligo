// storage type

type threshold is nat
type max_proposal is nat
type max_message_size is nat
type state_hash is bytes
type addr_set is set (address)
type message_store is map (bytes, addr_set)
type proposal_counters is map (address, nat)

type storage is
  record [
    state_hash           : state_hash;
    threshold            : threshold;
    max_proposal         : max_proposal;
    max_message_size     : max_message_size;
    authorized_addresses : addr_set;
    message_store        : message_store;
    proposal_counters    : proposal_counters
  ]

// I/O types

type message is bytes -> list (operation)
type send_pt is message
type withdraw_pt is message
type default_pt is unit

type return is list (operation) * storage

type parameter is
  Send     of send_pt
| Withdraw of withdraw_pt
| Default  of default_pt

function send (const param : send_pt; var s : storage) : return is
  block {
    // check sender against the authorized addresses

    if not Set.mem (Tezos.sender, s.authorized_addresses)
    then failwith("Unauthorized address")
    else skip;

    // check message size against the stored limit

    var message : message := param;
    const packed_msg : bytes = Bytes.pack (message);
    if Bytes.length (packed_msg) > s.max_message_size
    then failwith ("Message size exceed maximum limit")
    else skip;

    (* compute the new set of addresses associated with the message and
       update counters *)

    var new_store : addr_set := set [];

    case map_get (packed_msg, s.message_store) of
      Some (voters) ->
        block {
          (* The message is already stored.
             Increment the counter only if the sender is not already
             associated with the message. *)
          if Set.mem (Tezos.sender, voters)
          then skip
          else s.proposal_counters[Tezos.sender] :=
                 get_force (Tezos.sender, s.proposal_counters) + 1n;
                 new_store := Set.add (Tezos.sender,voters)
        }
    | None ->
        block {
          // the message has never been received before
          s.proposal_counters[sender] :=
             get_force (Tezos.sender, s.proposal_counters) + 1n;
             new_store := set [Tezos.sender]
        }
    end;

    // check sender counters against the maximum number of proposal

    var sender_proposal_counter : nat :=
      get_force (Tezos.sender, s.proposal_counters);

    if sender_proposal_counter > s.max_proposal
    then failwith ("Maximum number of proposal reached")
    else skip;

    // check the threshold

    var ret_ops : list (operation) := nil;

    if Set.cardinal (new_store) >= s.threshold then {
      remove packed_msg from map s.message_store;
      ret_ops := message (s.state_hash);
      // update the state hash
      s.state_hash := Crypto.sha256 (Bytes.concat (s.state_hash, packed_msg));
      // decrement the counters
      for addr -> ctr in map s.proposal_counters block {
        if Set.mem (addr, new_store) then
          s.proposal_counters[addr] := abs (ctr - 1n)
        else skip
      }
    } else s.message_store[packed_msg] := new_store
  } with (ret_ops, s)

function withdraw (const param : withdraw_pt; var s : storage) : return is
  block {
    var message : message := param;
    const packed_msg : bytes = Bytes.pack (message);

    case s.message_store[packed_msg] of
      Some (voters) ->
        block {
          // The message is stored
          const new_set : addr_set = Set.remove (Tezos.sender, voters);

          (* Decrement the counter only if the sender was already
             associated with the message *)

          if Set.cardinal (voters) =/= Set.cardinal (new_set)
          then s.proposal_counters[Tezos.sender] :=
                 abs (get_force (Tezos.sender, s.proposal_counters) - 1n)
          else skip;

          (* If the message is left without any associated addresses,
             remove the corresponding message_store field *)

          if Set.cardinal (new_set) = 0n
          then remove packed_msg from map s.message_store
          else s.message_store[packed_msg] := new_set
        }
    | None -> skip
    end // The message is not stored, ignore.
  } with ((nil : list (operation)), s)

function default (const _ : default_pt; const s : storage) : return is
    ((nil : list (operation)), s)

function main (const param : parameter; const s : storage) : return  is
  case param of
    (* Propagate message p if the number of authorized addresses having
       voted for the same message p equals the threshold. *)
    | Send (p) -> send (p, s)

    (* Withraw vote for message p *)
    | Withdraw (p) -> withdraw (p, s)

    (* Use this action to transfer tez to the contract *)
    | Default (p) -> default (p, s)
  end
