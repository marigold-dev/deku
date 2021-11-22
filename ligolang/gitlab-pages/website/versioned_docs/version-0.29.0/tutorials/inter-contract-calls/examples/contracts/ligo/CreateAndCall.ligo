function create_and_call (const st : list (address)) is
block {
  const create_contract_result = 
      Tezos.create_contract(
          (function (const p : int; const s : int) is
            ((list [] : list (operation)), p + s)),
          (None : option (key_hash)),
          0mutez,
          1
      );
  const create_op = create_contract_result.0;
  const addr = create_contract_result.1;
  const call_op =
      Tezos.transaction(
          (addr, 41),
          0mutez,
          (Tezos.self ("%callback") : contract (address * int))
      )
} with (list [create_op; call_op], (addr # st))

function call_counter (const addr : address; const n : int) is
block {
  assert (Tezos.sender = Tezos.self_address);
  const callee_opt : option (contract (int)) = Tezos.get_contract_opt (addr);
  const callee =
      case callee_opt of
      |  Some (contract) -> contract
      | None -> (failwith ("Could not find contract") : contract (int))
      end
} with Tezos.transaction (n, 0mutez, callee)

type parameter is
| Callback of address * int
| CreateAndCall

function main (const param : parameter; const st : list (address)) is
  case param of
  | CreateAndCall -> create_and_call (st)
  | Callback (vs) -> (list [call_counter (vs)], st)
  end
