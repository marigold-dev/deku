import { FA2_Error, update_operator_type } from "../src/fa2";
import * as child_process from 'child_process';

const alice = "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy";
const bob = "tz1XPCN4m8m8YWW6qWz18eYN68n4mc8rj4ZQ";
const john = "tz1dn6Pf8VMNAhwBSStZGTxsqa18sapKFrMt";
const token_id = 1

const initial_state = {
  [`${alice}-${token_id}`]:{ amount: 1, operators: [] },
  [`${alice}-${2}`]:{ operators: [bob], amount: 1 },
  [`${bob}-${2}`]:{ operators: [john], amount: 1 },
  [`${john}-${2}`]:{ operators: [alice], amount: 1 },
  ["1"]:{ "": "", name: "my-token", symbol: "TT", decimals: "" },
  ["2"]:{ "": "", name: "my-second-token", symbol: "TT", decimals: "" },
  ["3"]:{ "": "", name: "my-third-token", symbol: "TT", decimals: "" },
  ["administrator"]:{ value: alice },
}

type result = { isOk: boolean, error: string, next_state: Record<string, unknown> }

const execute = (state: any, operation: any): result => {
  const { stdout, stderr } = child_process.spawnSync("deku-cli", ["create-mock-transaction", './wallet.json', JSON.stringify(operation), 'node', './build/main.js'], { 
    env: {
      ...process.env, 
      'NODE_ENV':'test',
      'TEST_STATE': JSON.stringify(state)
    } 
  });

  const next_state = stdout.toString().trim().split("\n").pop(); // The last line of stdout is the new state
  const error = stderr.toString().trim().split("\n").find(err => err.includes('VM error'))?.split(': ')[1] // Parsing the error to get the error message
  const parsed_next_state = JSON.parse(next_state)
    .reduce((acc, element) => {
      acc[element[0]]=element[1];
      return acc
    }, {});

  return { isOk: !error, error, next_state: parsed_next_state };
}

// some helper functions

const create_transfer = (from, to, token_id, amount) => ({ type: "transfer", op: [{ from, txs: [{ to, token: { token_id, amount } }] }] });
const create_update_operator = (owner, operator, token_id, type = update_operator_type.Add) => ({ type: "update_operators", op: [{ type, operator: { owner, 1: { operator, token_id } } }] });
const create_mint_token = (new_token_id) => ({ type: "mint_token", op: [{ token_id: new_token_id, amount: 5, token_metadata: { "": "", name: "my-new-token", symbol: "TT", decimals: "" } }] })

const empty_operation = { type: "transfer", op: [] };

const get_token_amount = (state, address, token_id) => {
  const ledger_entry = state[`${address}-${token_id}`];
  const token_amount = ledger_entry?.amount;
  return token_amount ? token_amount : 0;
}

const get_operators = (state, address, token_id) => {
  const ledger_entry = state[`${address}-${token_id}`];
  const operators = ledger_entry?.operators;
  return operators ? operators : [];
}

test('If one transfer fails, the whole transaction MUST fail, the state MUST remain unchanged', () => {
  const alice_initial_token_amount = get_token_amount(initial_state, alice, token_id);

  const result = execute(initial_state, create_transfer(alice, bob, token_id, 2));

  expect(result.isOk).toBeFalsy();
  expect(result.error).toBe(FA2_Error.FA2_INSUFFICIENT_BALANCE);
  expect(get_token_amount(result.next_state, alice, token_id)).toBe(alice_initial_token_amount);
});

test('Each transfer must decrement the amount of token of "_from" and increment the amount of token of "to"', () => {
  const alice_initial_token_amount = get_token_amount(initial_state, alice, token_id);
  const bob_initial_token_amount = get_token_amount(initial_state, bob, token_id);
  const amount_of_transfer = 1;

  const result = execute(initial_state, create_transfer(alice, bob, token_id, amount_of_transfer));

  expect(result.isOk).toBeTruthy();
  expect(get_token_amount(result.next_state, alice, token_id)).toBe(alice_initial_token_amount - amount_of_transfer);
  expect(get_token_amount(result.next_state, bob, token_id)).toBe(bob_initial_token_amount + amount_of_transfer);
});

test('If the transfer amount exceeds current token balance of the source address, the whole transfer operation MUST fail with the error mnemonic "FA2_INSUFFICIENT_BALANCE"', () => {
  const result = execute(initial_state, create_transfer(alice, bob, token_id, 2));
  expect(result.isOk).toBeFalsy();
  expect(result.error).toBe(FA2_Error.FA2_INSUFFICIENT_BALANCE);
});

test('If the owner has no token, the amount of this token is interpreted as 0', () => {
  const result = execute(initial_state, empty_operation);
  expect(result.isOk);
  expect(get_token_amount(result.next_state, bob, token_id)).toBe(0);
});

test('The transfer MUST update token balances exactly as the operation parameters specify it', () => {
  const alice_initial_token_amount = get_token_amount(initial_state, alice, token_id);
  const bob_initial_token_amount = get_token_amount(initial_state, bob, token_id);
  const amount_of_transfer = 1

  const result = execute(initial_state, create_transfer(alice, bob, token_id, amount_of_transfer));

  const alice_next_token_amount = get_token_amount(result.next_state, alice, token_id);
  const bob_next_token_amount = get_token_amount(result.next_state, bob, token_id);

  expect(alice_initial_token_amount - alice_next_token_amount).toBe(amount_of_transfer);
  expect(bob_next_token_amount - bob_initial_token_amount).toBe(amount_of_transfer);
});

test('Transfers of zero amount MUST be treated as normal transfers', () => {
  const amount_of_transfer = 0;
  const result = execute(initial_state, create_transfer(alice, bob, token_id, amount_of_transfer));
  expect(result.isOk).toBeTruthy()
});

test('Transfer with the same address (from_ equals to_) MUST be treated as normal transfers', () => {
  const alice_initial_token_amount = get_token_amount(initial_state, alice, token_id);
  const amount_of_transfer = 1;

  const result = execute(initial_state, create_transfer(alice, alice, token_id, amount_of_transfer));

  expect(get_token_amount(result.next_state, alice, token_id)).toBe(alice_initial_token_amount);
});

test('If one of the token_id is not defined within the FA2 contract, the entrypoint MUST fail with the error mnemonic FA2_TOKEN_UNDEFINED', () => {
  const result = execute(initial_state, create_transfer(alice, bob, 42, 1));
  expect(result.error).toBe(FA2_Error.FA2_TOKEN_UNDEFINED);
});

test('Token owner address MUST be able to perform a transfer of its own tokens', () => {
  const result = execute(initial_state, create_transfer(alice, alice, 1, 1));
  expect(result.isOk).toBeTruthy();
});

test('An operator MUST be permitted to manage the specified owners tokens vefore it invokes a transfer transaction', () => {
  const result = execute(initial_state, create_transfer(alice, bob, 2, 1));
  expect(result.isOk).toBeTruthy();
});

test('If the address that invokes a transfer operation is neither a token owner not one of the operators, the transaction MUST fail with the error mnemonic "FA2_NOT_OPERATOR"', () => {
  const result = execute(initial_state, create_transfer(bob, alice, 2, 1));
  expect(result.isOk).toBeFalsy();
  expect(result.error).toBe(FA2_Error.FA2_NOT_OPERATOR);
});

test('The FA2 contract MAY limit operator updates to a token owner (owner == SENDER)', () => {
  const result = execute(initial_state, create_update_operator(bob, alice, 2));
  expect(result.isOk).toBeFalsy();
  expect(result.error).toBe(FA2_Error.FA2_NOT_OWNER);
});

test('The entrypoint accepts a list of update_operator commands. If two different commands in the list add and remove an operator for the same token owner and token ID, the last command in the list MUST take effect.', () => {
  const alices_operators = get_operators(initial_state, alice, token_id);

  const _result1 = execute(initial_state, create_update_operator(alice, bob, token_id));
  const result2 = execute(initial_state, create_update_operator(alice, bob, token_id, update_operator_type.Remove));

  expect(alices_operators).toStrictEqual(get_operators(result2.next_state, alice, token_id));
});

test("It is possible to update operators for a token owner that does not hold any token balances yet.", () => {
  const alices_operators = get_operators(initial_state, alice, 3);
  const alice_token_amount = get_token_amount(initial_state, alice, 3);

  const result = execute(initial_state, create_update_operator(alice, bob, token_id));

  expect(alice_token_amount).toBe(0);
  expect(alices_operators).toStrictEqual([]);
  expect(get_operators(result.next_state, alice, token_id)).toContain(bob);
});

test('Operator relation is not transitive. If C is an operator of B and if B is an operator of A, C cannot transfer tokens that are owned by A, on behalf of B.', () => {
  const result = execute(initial_state, create_transfer(bob, alice, 2, 1));
  expect(result.error).toBe(FA2_Error.FA2_NOT_OPERATOR);
});

test('A administrator can mint a token', () => {
  const new_token_id = 42;
  const result = execute(initial_state, create_mint_token(new_token_id));
  expect(result.isOk).toBeTruthy();
});

test('The owner of a minted token is the administrator', () => {
  const new_token_id = 42;
  const alices_new_token = get_token_amount(initial_state, alice, new_token_id);
  const result = execute(initial_state, create_mint_token(new_token_id));

  expect(alices_new_token).toBe(0);
  expect(result.isOk).toBeTruthy();
  expect(get_token_amount(result.next_state, alice, new_token_id)).not.toBe(alices_new_token);
});