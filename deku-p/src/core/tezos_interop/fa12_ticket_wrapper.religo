// To deposit FA1.2 and recieve tickets:
//
//   1) Pass `Allow(...)` to the FA1.2 contract's transfer entry point, allowing this contract to transfer the desired amount of tokens.
//
//      We will use this to attempt to transfer the tokens from your address to ours. If you don't do this, the transfer will fail, and everything will be rolled back.
//
//   2) Using the same wallet address the FA1.2 tokens were associated with, pass `Deposit(...)` to this contract's main entry point.
//
//   3) A callback is taken. It will be invoked and passed the tickets corresponding to your deposit. If the callback cannot be invoked, the operation will fail, and the original transfer of tickets will be rolled back.
//
// To withdraw FA1.2 in exchange for tickets:
//
//   1) Send the tickets to this contract's main entry point inside `Withdraw(...)`. Corresponding FA1.2 tokens will be sent to the address that sent the tickets.

type transfer = contract((address, (address, nat)));

type desposit_op = {
  fa12_transfer: transfer,
  amount_to_transfer: nat,
  callback: contract(ticket(bytes)),
};

type parameter =
  | Deposit(desposit_op)
  | Withdraw(ticket(bytes));

type return = (list(operation), unit);

let assert_msg = (message: string, condition: bool) =>
  if (!condition) {
    failwith(message);
  };

// Example contract at https://gitlab.com/camlcase-dev/fa1.2/-/blob/master/ligo/fa1.2.ligo
let get_transfer_contract = (fa12_contract_addr: address) => {
  let transfer_opt: option(transfer) =
    Tezos.get_entrypoint_opt("%transfer", fa12_contract_addr);
  let transfer =
    switch (transfer_opt) {
    | Some(contract) => contract
    | None => (
        failwith(
          "The contract does not exist or is not a valid fa1.2 contract"
        ): transfer
      )
    };
  transfer;
};

let get_funds = (fa12_transfer: transfer, amt: nat) => {
  Tezos.transaction(
    (Tezos.source, (Tezos.self_address, amt)),
    0mutez,
    fa12_transfer
  );
};

let deposit = (op: desposit_op): list(operation) => {
  // The type `transfer` represents a reference to a contract, not a contract itself.
  // So you'd think you could just store the contract, but that doesn't work because you can't deserialzie from bytes to a contract
  // (https://ide.ligolang.org/p/-JhY5EHnjQkIDO6m7NhNyA)
  // So we serialize the address and store that instead.
  let tic: ticket(bytes) =
    Tezos.create_ticket(
      Bytes.pack(Tezos.address(op.fa12_transfer)),
      op.amount_to_transfer
    );
  let give_tic = Tezos.transaction(tic, 0mutez, op.callback);
  [get_funds(op.fa12_transfer, op.amount_to_transfer), give_tic];
};

let withdraw = (ticket: ticket(bytes)) => {
  // This discards the ticket
  let ((ticket_address, (fa12_transfer_contract_address_bytes, amount_)), _) =
    Tezos.read_ticket(ticket);

  assert_msg(
    "We only accept tickets we created!",
    ticket_address == Tezos.self_address
  );

  let fa12_transfer_address_opt: option(address) =
    Bytes.unpack(fa12_transfer_contract_address_bytes);
  let fa12_transfer_address =
    switch (fa12_transfer_address_opt) {
    | Some(fa12_transfer_address) => fa12_transfer_address
    | None => (
        failwith("The ticket somehow did not contain a valid address"): address
      )
    };
  let fa12_transfer = get_transfer_contract(fa12_transfer_address);
  let op =
    Tezos.transaction(
      (Tezos.self_address, (Tezos.source, amount_)),
      0mutez,
      fa12_transfer
    );
  [op];
};

let main = (action: parameter, _store: unit): return => (
  switch (action) {
  | Deposit(op) => deposit(op)
  | Withdraw(ticket) => withdraw(ticket)
  },
  ()
);
