// Copyright Coase, Inc 2019

type card_pattern_id is nat

type card_pattern is record [
  coefficient : tez;
  quantity    : nat
]

type card_patterns is map (card_pattern_id, card_pattern)

type card_id is nat

type card is record [
  card_owner   : address;
  card_pattern : card_pattern_id
]

type cards is map (card_id, card)

type storage is record [
  cards         : cards;
  card_patterns : card_patterns;
  next_id       : nat
]

type return is list (operation) * storage

type action_buy_single is record [
  card_to_buy : card_pattern_id
]

type action_sell_single is record [
  card_to_sell : card_id
]

type action_transfer_single is record [
  card_to_transfer : card_id;
  destination      : address
]

type parameter is
  Buy_single      of action_buy_single
| Sell_single     of action_sell_single
| Transfer_single of action_transfer_single

function transfer_single (const action : action_transfer_single;
                          var s : storage) : return is
  block {
    var cards : cards := s.cards;
    var card : card :=
      case cards[action.card_to_transfer] of
        Some (card) -> card
      | None -> (failwith ("transfer_single: No card.") : card)
      end;
    if card.card_owner =/= sender then
      failwith ("This card doesn't belong to you")
    else skip;
    card.card_owner := action.destination;
    cards[action.card_to_transfer] := card;
    s.cards := cards
  } with ((nil : list (operation)), s)

function sell_single (const action : action_sell_single;
                      var s : storage) : return is
  block {
    const card : card =
      case s.cards[action.card_to_sell] of
        Some (card) -> card
      | None -> (failwith ("sell_single: No card.") : card)
      end;
    if card.card_owner =/= sender
    then failwith ("This card doesn't belong to you")
    else skip;
    var card_pattern : card_pattern :=
      case s.card_patterns[card.card_pattern] of
        Some (pattern) -> pattern
      | None -> (failwith ("sell_single: No card pattern.") : card_pattern)
      end;
    card_pattern.quantity := abs (card_pattern.quantity - 1n);
    var card_patterns : card_patterns := s.card_patterns;
    card_patterns[card.card_pattern] := card_pattern;
    s.card_patterns := card_patterns;
    var cards : cards := s.cards;
    remove action.card_to_sell from map cards;
    s.cards := cards;
    const price : tez = card_pattern.coefficient * card_pattern.quantity;
    const receiver : contract (unit) =
      case (Tezos.get_contract_opt (Tezos.sender) : option (contract (unit))) of
        Some (contract) -> contract
      | None -> (failwith ("sell_single: No contract.") : contract (unit))
      end;
    const op : operation = Tezos.transaction (unit, price, receiver);
    const operations : list (operation) = list [op]
  } with (operations, s)

function buy_single (const action : action_buy_single;
                     var s : storage) : return is
  block {
    // Check funds
    var card_pattern : card_pattern :=
      case s.card_patterns[action.card_to_buy] of
        Some (pattern) -> pattern
      | None -> (failwith ("buy_single: No card pattern.") : card_pattern)
      end;
    const price : tez =
      card_pattern.coefficient * (card_pattern.quantity + 1n);
    if price > amount then failwith ("Not enough money") else skip;
    // Increase quantity
    card_pattern.quantity := card_pattern.quantity + 1n;
    var card_patterns : card_patterns := s.card_patterns;
    card_patterns[action.card_to_buy] := card_pattern;
    s.card_patterns := card_patterns;
    // Add card
    var cards : cards := s.cards;
    cards[s.next_id] := record [
      card_owner   = sender;
      card_pattern = action.card_to_buy
    ];
    s.cards := cards;
    s.next_id := s.next_id + 1n
  } with ((nil : list (operation)), s)

function main (const action : parameter; const s : storage) : return is
  case action of
    Buy_single (bs)      -> buy_single (bs, s)
  | Sell_single (as)     -> sell_single (as, s)
  | Transfer_single (at) -> transfer_single (at, s)
  end
