(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_builder_fungible.tz

Goes with ticket_wallet.mligo.
*)

type mint_parameter =
  [@layout:comb]
  {destination : unit ticket contract;
   amount : nat}

type parameter =
  | Burn of unit ticket
  | Mint of mint_parameter

type storage =
  [@layout:comb]
  {admin : address}

let main (arg : parameter * storage) : operation list * storage =
  begin
    assert (Tezos.amount = 0mutez);
    let (p,s) = arg in
    match p with
    | Burn ticket ->
      begin
        let ((ticketer, _), ticket) = (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
        assert (ticketer = Tezos.self_address);
        (([] : operation list), s)
      end
    | Mint mint ->
      begin
        assert (Tezos.sender = s.admin);
        let ticket = Tezos.create_ticket () mint.amount in
        let op = Tezos.transaction ticket 0mutez mint.destination in
        ([op], s)
      end
  end
