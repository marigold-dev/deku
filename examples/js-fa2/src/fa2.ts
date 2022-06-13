"use strict";

type nat = number; // TODO: create a real constructor to avoid negative number
type address = string;
type token_id = nat;
type amount = nat;
type token_metadata = {
  "": string,
  name: string,
  symbol: string,
  decimals: string, // I don't understand the given definition...
}
// TODO: review the name
// Just use to type what is stored in the deku state
type ledger_entry = {
  amount: amount,
  operators: address[]
}

// All the possible errors
export enum FA2_Error {
  FA2_TOKEN_UNDEFINED = "fa2_token_undefined", // One of the specified token_ids is not defined within the FA2 contract
  FA2_INSUFFICIENT_BALANCE = "fa2_insufficient_balance", // A token owner does not have sufficient balance to transfer tokens from owner's account
  FA2_TX_DENIED = "fa2_tx_denied",// A transfer failed because of operator_transfer_policy == No_transfer
  FA2_NOT_OWNER = "fa2_not_owner", // A transfer failed because operator_transfer_policy == Owner_transfer and it is invoked not by the token owner
  FA2_NOT_OPERATOR = "fa2_not_operator", // A transfer failed because operator_transfer_policy == Owner_or_operator_transfer and it is invoked neither by the token owner nor a permitted operator
  FA2_OPERATORS_UNSUPPORTED = "fa2_operators_unsupported", // update_operators entrypoint is invoked and operator_transfer_policy is No_transfer or Owner_transfer
  FA2_RECEIVER_HOOK_FAILED = "fa2_receiver_hook_failed", // The receiver hook failed. This error MUST be raised by the hook implementation
  FA2_SENDER_HOOK_FAILED = "fa2_sender_hook_failed", // The sender failed. This error MUST be raised by the hook implementation
  FA2_RECEIVER_HOOK_UNDEFINED = "fa2_receiver_hook_undefined", // Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
  FA2_SENDER_HOOK_UNDEFINED = "fa2_sender_hook_undefined", // Sender hook is required by the permission behavior, but is not implemented by a sender contract
}

export enum FA2_Custom_Error {
  FA2_TOKEN_ALREADY_EXISTS = "fa2_token_already_exists" // If the token already exists (used in the mint operation) TODO: See the standard for custom errors
}

export type Deku_storage = {
  get: (key: string) => string | undefined
  set: (key: string, value: string) => void
}

type fa2_storage = {
  // returns the amount of token holds by the given address
  get_token_amount: (address: address, token_id: token_id) => amount;
  // returns true if the token exists, false otherwise
  exists: (token_id: token_id) => boolean;
  // decrements the token balance of a given amount
  decrement_token_balance: (address: address, token_id: token_id, amount: amount) => void;
  // increments the token balance of a gieven amount
  increment_token_balance: (address: address, token_id: token_id, amount: amount) => void;
  // adds an operator to a token
  add_operator: (owner: address, token_id: token_id, operator: address) => void;
  // removes an operator to a token
  remove_operator: (owner: address, token_id: token_id, operator: address) => void;
  // returns all the operator for a token
  get_operators: (address: address, token_id: token_id) => address[];
  // returns the metadata on a token
  get_token_metadata: (token_id: token_id) => token_metadata
  // returns the administrator of the fa2 storage
  get_administrator: () => address | undefined
  // add a token in the store
  add_token: (token_id: token_id, token_metadata: token_metadata, amount: amount) => void;
}

// This function is used to create a fa2_storage (see the interface) form the Deku_storage
// It's a kind of wrapper arround the deku_storage
export const fa2_storage = (deku_storage: Deku_storage): fa2_storage => {
  const get_token_amount = (address: address, token_id: token_id): amount => {
    const json_str: string | undefined = deku_storage.get(`${address}-${token_id}`);
    if (json_str === undefined) {
      return 0 // If the address does not own the token, the amount of token is considered to be 0
    }
    const json: { operators: address[], amount: amount } = JSON.parse(json_str);
    return json.amount
  }

  const exists = (token_id: token_id): boolean => {
    const json_str = deku_storage.get(`${token_id}`);
    return json_str !== undefined
  }

  const decrement_token_balance = (address: address, token_id: token_id, amount: amount) => {
    const json_str = deku_storage.get(`${address}-${token_id}`);
    if (json_str === undefined) {
      throw FA2_Error.FA2_INSUFFICIENT_BALANCE
    }
    const json = JSON.parse(json_str);
    const next = { ...json, amount: json.amount - amount };
    deku_storage.set(`${address}-${token_id}`, JSON.stringify(next));
  }

  const increment_token_balance = (address: address, token_id: token_id, amount: amount) => {
    const json_str = deku_storage.get(`${address}-${token_id}`);
    const json: ledger_entry = json_str === undefined
      ? { operators: [], amount: 0 }
      : JSON.parse(json_str);
    const next = { ...json, amount: json.amount + amount }
    deku_storage.set(`${address}-${token_id}`, JSON.stringify(next));
  }

  const add_operator = (owner: address, token_id: token_id, operator: address) => {
    const json_str = deku_storage.get(`${owner}-${token_id}`);
    const json: ledger_entry = json_str === undefined
      ? { amount: 0, operators: [] }
      : JSON.parse(json_str)
    const next = { ...json, operators: [...json.operators, operator] }
    deku_storage.set(`${owner}-${token_id}`, JSON.stringify(next));
  }

  const remove_operator = (owner: address, token_id: token_id, operator: address) => {
    const json_str = deku_storage.get(`${owner}-${token_id}`);
    if (json_str === undefined) {
      return;
    } else {
      const json: ledger_entry = JSON.parse(json_str);
      const next = { ...json, operators: json.operators.filter(op => op !== operator) }
      deku_storage.set(`${owner}-${token_id}`, JSON.stringify(next));
    }
  }

  const get_operators = (address: address, token_id: token_id): address[] => {
    const json_str = deku_storage.get(`${address}-${token_id}`);
    if (json_str === undefined) {
      return []
    } else {
      const json = JSON.parse(json_str);
      return json.operators
    }
  }

  const get_token_metadata = (token_id: token_id): token_metadata => {
    const json_str = deku_storage.get(`${token_id}`);
    if (json_str === undefined) {
      throw FA2_Error.FA2_TOKEN_UNDEFINED
    } else {
      return JSON.parse(json_str);
    }
  }

  const get_administrator = (): address | undefined => {
    const json_str = deku_storage.get('administrator');

    if (json_str === undefined) {
      return undefined
    } else {
      const admin = JSON.parse(json_str).value;
      return admin;
    }
  }

  const add_token = (token_id: token_id, token_metadata: token_metadata, token_amount: amount) => {
    if (deku_storage.get(`${token_id}`) !== undefined) {
      throw FA2_Custom_Error.FA2_TOKEN_ALREADY_EXISTS
    }
    const administrator = get_administrator();
    if (administrator === undefined) {
      throw FA2_Error.FA2_NOT_OPERATOR;
    }

    deku_storage.set(`${token_id}`, JSON.stringify(token_metadata));
    deku_storage.set(`${administrator}-${token_id}`, JSON.stringify({ amount: token_amount, operators: [] }));
  }

  return {
    get_token_amount,
    exists,
    decrement_token_balance,
    increment_token_balance,
    add_operator,
    remove_operator,
    get_operators,
    get_token_metadata,
    get_administrator,
    add_token
  }
}

// Transfer entry point
type tx = {
  to: address,
  token: {
    token_id: token_id,
    amount: amount
  }
}

type transfer_type = {
  from: address,
  txs: tx[]
}

type transfer_operation = transfer_type[];

export const transfer = (transfers: transfer_operation, sender: address, token_storage: fa2_storage): void => {
  const _next_balance = transfers.reduce<{ [key: string]: amount | undefined }>((global_acc, transfer) => {
    const { from, txs } = transfer;

    txs.forEach(tx => {
      const { to, token: { token_id, amount } } = tx;

      // Check if the token exists
      if (!token_storage.exists(token_id)) {
        throw FA2_Error.FA2_TOKEN_UNDEFINED
      }

      const from_balance = global_acc[`${from}-${token_id}`] === undefined ? token_storage.get_token_amount(from, token_id) : global_acc[`${from}-${token_id}`]
      // Check if the user as enough balance
      if (from_balance < amount) {
        throw FA2_Error.FA2_INSUFFICIENT_BALANCE
      }

      // Check policies
      const operators = token_storage.get_operators(from, token_id);
      const is_operator = operators.find((operator_addr) => sender === operator_addr) !== undefined
        || sender === from
      if (!is_operator) {
        throw FA2_Error.FA2_NOT_OPERATOR
      }

      const to_balance = global_acc[`${to}-${token_id}`] === undefined ? token_storage.get_token_amount(to, token_id) : global_acc[`${to}-${token_id}`]

      // Compute the next token amount for each address
      global_acc[`${from}-${token_id}`] = from_balance - amount;
      global_acc[`${to}-${token_id}`] = to_balance + amount;
    });
    return global_acc
  }, {});


  // if the next line is executed it means there wasn't any errors in the previous step so we can decrement and increment token balance
  transfers.forEach(transfer => {
    const { from, txs } = transfer;

    // Now we can modify our storage
    txs.forEach((tx: tx) => {
      const { to, token: { token_id, amount } } = tx;
      token_storage.decrement_token_balance(from, token_id, amount); // See how to rollback if one of this function fail, maybe a solution is to set 
      token_storage.increment_token_balance(to, token_id, amount);
    });
  });
}

// update_operator entry point

type update_operator = {
  owner: address,
  1: {
    operator: address,
    token_id: token_id
  }
}

export enum update_operator_type {
  Add = 'add',
  Remove = 'remove'
}

type update_operators_operation = {
  type: update_operator_type,
  operator: update_operator
}[]

// The type of the input of the main function
enum operation_type {
  Transfer = "transfer",
  Balance_of = "balance_of",
  Update_operators = "update_operators",
  Mint_token = "mint_token"
}


export const update_operators = (update_operators_input: update_operators_operation, sender: address, token_storage: fa2_storage): void => {
  update_operators_input.forEach(update_operator => {
    // TODO: we may add an admin as operator of everything
    if (update_operator.operator.owner !== sender) {
      throw FA2_Error.FA2_NOT_OWNER
    }

    switch (update_operator.type) {
      case update_operator_type.Add: {
        token_storage.add_operator(update_operator.operator.owner, update_operator.operator[1].token_id, update_operator.operator[1].operator)
        return;
      }
      case update_operator_type.Remove: {
        token_storage.remove_operator(update_operator.operator.owner, update_operator.operator[1].token_id, update_operator.operator[1].operator)
        return;
      }
    }
  })
}


// balance of entrypoint
// This implementation is not the same as TZIP-2 because I don't know in typescript how to differentiate types in an union type

type balance_request = { owner: address, token_id: token_id };
type balance_response = { request: balance_request, balance: amount };

type contract<_CallbackType> = { address: address };
type callback = contract<balance_response>;

type balance_of_operation = {
  requests: balance_request[],
  callback: callback
}
export const balance_of = (balance_of_input: balance_of_operation, token_storage: fa2_storage): void => {
  const _contract_callback_address = balance_of_input.callback.address;

  const balance_response = balance_of_input.requests
    .map(request => ({
      request,
      balance: token_storage.get_token_amount(request.owner, request.token_id)
    }));

  // TODO: How to send the response back ? For now, The respose is printing
  console.log(balance_response);
}

// Entry point of mint_token operation

type mint_token = {
  token_metadata: token_metadata,
  token_id: token_id,
  amount: amount
}

type mint_token_operation = mint_token[]

export const mint_token = (mint_token_operation: mint_token_operation, sender: address, token_storage: fa2_storage): void => {
  const admin = token_storage.get_administrator();
  if (admin !== sender) {
    throw FA2_Error.FA2_NOT_OPERATOR
  }

  mint_token_operation.forEach(mint_token => {
    const { token_id, token_metadata, amount } = mint_token;
    // Verify if the token already exists
    if (token_storage.exists(token_id)) {
      throw FA2_Custom_Error.FA2_TOKEN_ALREADY_EXISTS
    }
    token_storage.add_token(token_id, token_metadata, amount); // The administrator will own this token
  });
  return;
}

export const initial_state = {
  "1": { "": "", name: "my first token", symbol: 'AT1', decimals: "" },
  "2": { "": "", name: "my second token", symbol: 'AT2', decimals: "" },
  "3": { "": "", name: "my third token", symbol: 'AT3', decimals: "" },
  "administrator": { value: "tz1ZUHdFkMKwSEeSfYhN6e4CVUBxvfbHNA6L" },
  [`${"tz1c5ZDfNvSrfRFAsp6uPYvCaZhpviwYpBfy"}-1`]: { amount: 1, operators: [] },
}

export const transition_state = (deku_storage: Deku_storage) => ({ source, operation }): string | void => {
  try {
    const { type, op } = operation;
    const storage = fa2_storage(deku_storage);

    switch (type) {
      case operation_type.Balance_of:
        return balance_of(op as balance_of_operation, storage);
      case operation_type.Transfer:
        return transfer(op as transfer_operation, source, storage);
      case operation_type.Update_operators:
        return update_operators(op as update_operators_operation, source, storage);
      case operation_type.Mint_token: {
        return mint_token(op as mint_token_operation, source, storage);
      }
      default:
        return FA2_Error.FA2_OPERATORS_UNSUPPORTED;
    }
  } catch (err) {
    return err
  }
}