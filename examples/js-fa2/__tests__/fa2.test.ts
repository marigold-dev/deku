import { Deku_storage, FA2_Custom_Error, FA2_Error, fa2_storage, mint_token, transfer, update_operators, update_operator_type } from "../src/fa2";

const alice = "tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy";
const bob = "tz1XPCN4m8m8YWW6qWz18eYN68n4mc8rj4ZQ";
const john = "tz1dn6Pf8VMNAhwBSStZGTxsqa18sapKFrMt";
const token_id = 1

let state = {}
const mock_deku_storage: Deku_storage = {
  get: (key: string) => JSON.stringify(state[key]),
  set: (key: string, value: string) => state[key] = value
}
const storage = fa2_storage(mock_deku_storage);

// Just some helpers function to create operations
const create_transfer = (from, to, token_id, amount) => [{ from, txs: [{ to, token: { token_id, amount } }] }];
const create_update_operator = (owner, operator, token_id, type = update_operator_type.Add) => [{ type, operator: { owner, 1: { operator, token_id } } }]
const create_mint_token = (token_id) => [{ token_id, amount: 23, token_metadata: { "": "", name: "my-second-token", symbol: "TT", decimals: "" } }]

beforeEach(() => {
  state = {
    [`${alice}-${token_id}`]: { operators: [], amount: 1 },
    [`${alice}-${2}`]: { operators: [bob], amount: 1 },
    "1": { "": "", name: "my-token", symbol: "TT", decimals: "" },
    "2": { "": "", name: "my-second-token", symbol: "TT", decimals: "" },
    "administrator": { value: bob }
  };
});

test('If one transfer fails, the whole transaction MUST fail, the state MUST remain unchanged', () => {
  const alice_initial_token_amount = storage.get_token_amount(alice, token_id);

  expect(() => transfer(create_transfer(alice, bob, token_id, 2), alice, storage))
    .toThrow(FA2_Error.FA2_INSUFFICIENT_BALANCE);
  expect(storage.get_token_amount(alice, token_id)).toBe(alice_initial_token_amount);
});

test('Each transfer must decrement the amount of token of "_from" and increment the amount of token of "to"', () => {
  const alice_initial_token_amount = storage.get_token_amount(alice, token_id);
  const bob_initial_token_amount = storage.get_token_amount(bob, token_id);
  const amount_of_transfer = 1

  transfer(create_transfer(alice, bob, token_id, amount_of_transfer), alice, storage);

  expect(storage.get_token_amount(alice, token_id)).toBe(alice_initial_token_amount - amount_of_transfer);
  expect(storage.get_token_amount(bob, token_id)).toBe(bob_initial_token_amount + amount_of_transfer);
});

test('If the transfer amount exceeds current token balance of the source address, the whole transfer operation MUST fail with the error mnemonic "FA2_INSUFFICIENT_BALANCE"', () => {
  expect(() => transfer(create_transfer(alice, bob, token_id, 2), alice, storage))
    .toThrow(FA2_Error.FA2_INSUFFICIENT_BALANCE);
});

test('If the owner has no token, the amount of this token is interpreted as 0', () => {
  expect(storage.get_token_amount(bob, token_id)).toBe(0); // Maybe we have to test the balance entry point ?
});

test('The transfer MUST update token balances exactly as the operation parameters specify it', () => {
  const alice_initial_token_amount = storage.get_token_amount(alice, token_id);
  const bob_initial_token_amount = storage.get_token_amount(bob, token_id);
  const amount_of_transfer = 1

  transfer(create_transfer(alice, bob, token_id, amount_of_transfer), alice, storage);

  const alice_next_token_amount = storage.get_token_amount(alice, token_id);
  const bob_next_token_amount = storage.get_token_amount(bob, token_id);

  expect(alice_initial_token_amount - alice_next_token_amount).toBe(amount_of_transfer);
  expect(bob_next_token_amount - bob_initial_token_amount).toBe(amount_of_transfer);
});

test('Transfers of zero amount MUST be treated as normal transfers', () => {
  const amount_of_transfer = 0
  expect(() => transfer(create_transfer(alice, bob, token_id, amount_of_transfer), alice, storage)).not.toThrowError();
});

test('Transfer with the same address (from_ equals to_) MUST be treated as normal transfers', () => {
  const alice_initial_token_amount = storage.get_token_amount(alice, token_id);
  const amount_of_transfer = 1;

  transfer(create_transfer(alice, alice, token_id, amount_of_transfer), alice, storage);

  expect(storage.get_token_amount(alice, token_id)).toBe(alice_initial_token_amount);
});

test('If one of the token_id is not defined within the FA2 contract, the entrypoint MUST fail with the error mnemonic FA2_TOKEN_UNDEFINED', () => {
  expect(() => transfer(create_transfer(alice, bob, 42, 1), alice, storage)).toThrow(FA2_Error.FA2_TOKEN_UNDEFINED);
});

test('Token owner address MUST be able to perform a transfer of its own tokens', () => {
  expect(() => transfer(create_transfer(alice, bob, 2, 1), alice, storage)).not.toThrowError()
})

test('An operator MUST be permitted to manage the specified owners tokens vefore it invokes a transfer transaction', () => {
  expect(() => transfer(create_transfer(alice, bob, 2, 1), bob, storage)).not.toThrowError()
});

test('If the address that invokes a transfer operation is neither a token owner not one of the operators, the transaction MUST fail with the error mnemonic "FA2_NOT_OPERATOR"', () => {
  expect(() => transfer(create_transfer(alice, bob, token_id, 1), bob, storage)).toThrow(FA2_Error.FA2_NOT_OPERATOR);
});

test('The owner of a token can add an operator to its token', () => {
  const alices_operators = storage.get_operators(alice, token_id);

  update_operators(create_update_operator(alice, bob, token_id), alice, storage);

  expect(alices_operators).not.toContain(bob);
  expect(storage.get_operators(alice, token_id)).toContain(bob);
});

test('The FA2 contract MAY limit operator updates to a token owner (owner == SENDER)', () => {
  expect(() => update_operators(create_update_operator(alice, bob, token_id), bob, storage)).toThrow(FA2_Error.FA2_NOT_OWNER);
});

test('The entrypoint accepts a list of update_operator commands. If two different commands in the list add and remove an operator for the same token owner and token ID, the last command in the list MUST take effect.', () => {
  const alices_operators = storage.get_operators(alice, token_id);

  update_operators(create_update_operator(alice, bob, token_id), alice, storage);
  update_operators(create_update_operator(alice, bob, token_id, update_operator_type.Remove), alice, storage);

  expect(alices_operators).toStrictEqual(storage.get_operators(alice, token_id));
});

test("It is possible to update operators for a token owner that does not hold any token balances yet.", () => {
  const bobs_operators = storage.get_operators(bob, token_id);
  const bob_token_amount = storage.get_token_amount(bob, token_id);

  expect(() => update_operators(create_update_operator(bob, alice, token_id), bob, storage)).not.toThrow();

  expect(bob_token_amount).toBe(0);
  expect(bobs_operators).toStrictEqual([]);
  expect(storage.get_operators(bob, token_id)).toContain(alice);
});

test('Operator relation is not transitive. If C is an operator of B and if B is an operator of A, C cannot transfer tokens that are owned by A, on behalf of B.', () => {
  update_operators(create_update_operator(alice, bob, token_id), alice, storage);
  update_operators(create_update_operator(bob, john, token_id), bob, storage);

  expect(() => transfer(create_transfer(alice, bob, token_id, 1), john, storage)).toThrow(FA2_Error.FA2_NOT_OPERATOR);
});

test('A non administrator cannot mint a token', () => {
  expect(() => mint_token(create_mint_token(42), alice, storage)).toThrow(FA2_Error.FA2_NOT_OPERATOR);
});

test('Only the administrator can mint a token', () => {
  expect(() => mint_token(create_mint_token(42), bob, storage)).not.toThrow(FA2_Error.FA2_NOT_OPERATOR);
});

test('The owner of a minted token is the administrator', () => {
  const new_token_id = 42;
  const bobs_new_token = storage.get_token_amount(bob, new_token_id);
  expect(() => mint_token(create_mint_token(new_token_id), bob, storage)).not.toThrow(FA2_Error.FA2_NOT_OPERATOR);
  expect(bobs_new_token).toBe(0);
  expect(storage.get_token_amount(bob, new_token_id)).not.toBe(bobs_new_token)
});

test("An administrator can't mint a token which already exists", () => {
  expect(() => mint_token(create_mint_token(token_id), bob, storage)).toThrow(FA2_Custom_Error.FA2_TOKEN_ALREADY_EXISTS);
})