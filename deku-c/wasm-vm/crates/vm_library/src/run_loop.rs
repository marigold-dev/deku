use std::{borrow::Cow, cell::RefCell, rc::Rc};

use fnv::{FnvHashMap, FnvHashSet};

use crate::{
    arena::ARENA,
    compile,
    contract_address::ContractAddress,
    errors::{vm::VmError, VMResult},
    execution_result::ExecutionResult,
    incoming::InvokeManaged,
    instance::invoke_managed,
    managed::value::{FromOcamlV, Value},
    outgoing::{InitVec, SetBorrowed},
    path::Path,
    pipe::IO,
    state::{ContractType, LigoCode, LigoContractState, State},
    ticket_table::{Ticket, TicketTable},
    vm_client::{ClientMessage, Operation, Transaction},
    vm_server::{ServerMessage, TicketDeposit},
};
struct ExecutionState {
    pub state: State,
    pub to_revert: Vec<(String, Option<Value>)>,
    pub io: IO,
    pub ticket_table: Rc<RefCell<TicketTable>>,
}
pub fn run_loop(io: IO) {
    let state = State::default();
    let table = TicketTable::default();
    let to_revert: Vec<(String, Option<Value>)> = Vec::with_capacity(100);
    let mut context = ExecutionState {
        state,
        to_revert,
        io,
        ticket_table: Rc::new(RefCell::new(table)),
    };
    loop {
        {
            let mut mutable = context.ticket_table.as_ref().borrow_mut();
            mutable.table.clear();
            mutable.counter = 0;
        };
        context
            .to_revert
            .drain(0..)
            .for_each(|(addr, contract_type)| {
                context.state.reset(addr, contract_type);
            });
        let arena = unsafe { &mut ARENA };

        arena.clear();
        'inner: loop {
            let msg = context.io.read();
            match msg {
                ClientMessage::SetInitialState(x) => {
                    State::from_init(&mut context.state, x).expect("failed to init_state");
                }
                ClientMessage::GetInitialState => {
                    context.io.write(&ServerMessage::Init(InitVec(vec![])))
                }
                ClientMessage::Transaction(transaction) => {
                    log::info!("Received transaction {:?}", transaction);
                    match handle_transaction(&mut context, transaction, 0) {
                        Ok(_) => context.io.write(&ServerMessage::Stop),
                        Err(_) => break 'inner,
                    }
                }
                ClientMessage::NoopTransaction => {
                    log::info!("Received Noop transaction");
                    context.io.write(&ServerMessage::Stop);
                    break 'inner;
                }
                x => panic!("run_loop not supported, {:?}", x),
            }
            context.to_revert.clear();
        }
    }
}

fn handle_transaction(
    context: &mut ExecutionState,
    transaction: Transaction,
    mut gas_limit: u64,
) -> VMResult<u64> {
    let io = &mut context.io;
    if let Ok(op) = serde_json::from_str(&transaction.operation) {
        match op {
            Operation::Invoke {
                address,
                argument,
                gas_limit: op_gas_limit,
            } => {
                gas_limit = op_gas_limit;
                let mut tickets2: FnvHashSet<Ticket> = transaction
                    .tickets
                    .clone()
                    .into_iter()
                    .map(|(x, y)| Ticket::new(x, y))
                    .collect();
                context
                    .io
                    .write(&ServerMessage::TakeTickets(&address.address));
                'd: loop {
                    match context.io.read() {
                        ClientMessage::GiveTickets(ticket) => {
                            tickets2.extend(ticket.into_iter().map(|(x, y)| Ticket::new(x, y)));
                            break 'd;
                        }
                        ClientMessage::NoopTransaction => (),
                        _ => panic!("bad format"),
                    }
                }

                let new_limit = handle_invoke(
                    context,
                    transaction,
                    address,
                    argument.0,
                    gas_limit,
                    tickets2,
                )?;
                gas_limit = new_limit;
                Ok(())
            }
            Operation::Originate {
                module_,
                constants,
                initial_storage,
                entrypoints,
                source,
            } => {
                let addres = handle_originate(
                    context,
                    module_,
                    constants.into_iter().map(|(x, y)| (x, y.0)).collect(),
                    initial_storage.0,
                    transaction.operation_raw_hash.as_bytes().to_vec(),
                    transaction.source,
                    entrypoints,
                    source,
                )?;
                let address = contract_addr_to_string(&addres);
                context
                    .io
                    .write_with_fail(&ServerMessage::DepositTickets(TicketDeposit {
                        address: &address,
                        tickets: &transaction.tickets,
                    }))
                    .map_err(|err| VmError::RuntimeErr(err.to_string()))
            }
            Operation::Transfer { address, tickets } => context
                .io
                .write_with_fail(&ServerMessage::DepositTickets(TicketDeposit {
                    address: &address,
                    tickets: &tickets,
                }))
                .map_err(|err| VmError::RuntimeErr(err.to_string())),
        }?;
        Ok::<u64, VmError>(gas_limit)
    } else {
        io.write(&ServerMessage::Error(format!(
            "bad operation, failed to parse operation, {}",
            &transaction.operation
        )));
        Err(VmError::DeserializeErr("Bad transaction".to_owned()))
    }
}
fn handle_originate(
    context: &mut ExecutionState,
    module: String,
    constants: Vec<(u32, Value)>,
    initial_storage: Value,
    operation_hash: Vec<u8>,
    originated_by: String,
    entrypoints: Option<FnvHashMap<String, Vec<Path>>>,
    source: Option<LigoCode>,
) -> VMResult<ContractAddress> {
    let module = compile::compile_managed_module(module.as_bytes())?;
    let serialized = module
        .serialize()
        .map_err(|x| VmError::CompileErr(x.to_string()))?;
    let addr = ContractAddress::new(&operation_hash);
    let contract_type = ContractType::LigoContract(LigoContractState {
        self_: addr.clone(),
        originated_by,
        storage: Box::from(initial_storage),
        module: Some(Box::from(module)),
        serialized_module: serialized,
        constants,
        entrypoints,
        source,
    });

    let msg = SetBorrowed {
        key: &addr.address,
        value: &Cow::Borrowed(&contract_type),
    };
    match context.io.write_with_fail(&ServerMessage::Set(msg)) {
        Ok(()) => Ok(()),
        Err(_) => {
            context
                .io
                .write(&ServerMessage::Error("failed to set".to_owned()));
            Err(VmError::RuntimeErr("cant talk to host".to_owned()))
        }
    }?;
    context.state.set(addr.address.clone(), contract_type);
    context.to_revert.push((addr.address.clone(), None));
    Ok(addr)
}
pub fn contract_addr_to_string(c: &ContractAddress) -> String {
    c.address.clone()
}
fn handle_invoke(
    context: &mut ExecutionState,
    transaction: Transaction,
    address: ContractAddress,
    argument: Value,
    mut gas_limit: u64,
    tickets: FnvHashSet<Ticket>,
) -> VMResult<u64> {
    match context.state.get(&address.address) {
        Some(contract) => {
            let arg = argument.to_runtime_ticket(&mut context.ticket_table.as_ref().borrow_mut());

            let storage = Box::from(
                contract
                    .storage()
                    .clone()
                    .to_runtime_ticket(&mut context.ticket_table.as_ref().borrow_mut()),
            );
            let mut contract = contract;
            let initial_len = tickets.len();
            let check = FnvHashSet::from_iter(
                context
                    .ticket_table
                    .as_ref()
                    .borrow()
                    .table
                    .clone()
                    .into_iter(),
            );
            let check: FnvHashSet<_> = tickets.union(&check).collect();
            if initial_len > context.ticket_table.as_ref().borrow().counter
                && check.len() != initial_len
            {
                return Err(VmError::RuntimeErr("Ticket ownership error".to_owned()));
            }
            contract.init()?;
            context
                .to_revert
                .push((address.address.clone(), Some(contract.storage().clone())));
            let invoke_payload = InvokeManaged {
                table: Rc::clone(&context.ticket_table),
                mod_: contract.module().as_ref().unwrap().as_ref(),
                arg,
                entrypoint_path: &address.entrypoint.as_ref().map_or_else(
                    || None,
                    |x| {
                        let map = contract.entrypoints().as_ref()?;
                        map.get(&format!("%{}", x)).cloned()
                    },
                ),
                initial_storage: storage,
                constants: contract.constants(),
                source: transaction.source.clone(),
                sender: transaction
                    .sender
                    .unwrap_or_else(|| transaction.source.clone()),
                self_addr: address.address.clone(),
                gas_limit,
            };
            let self_addr = address.clone();
            match invoke_managed(invoke_payload) {
                Ok(ExecutionResult {
                    new_storage,
                    ops,
                    remaining_gas,
                }) => {
                    gas_limit = remaining_gas;
                    let mut to_return = vec![];
                    let serialized_storage = Box::from(new_storage.from_runtime_ticket(
                        &mut context.ticket_table.as_ref().borrow_mut(),
                        &mut to_return,
                    )?);
                    {
                        let address = contract_addr_to_string(&address);
                        context
                            .io
                            .write_with_fail(&ServerMessage::DepositTickets(TicketDeposit {
                                address: &address,
                                tickets: &to_return,
                            }))
                            .map_err(|x| VmError::RuntimeErr(x.to_string()))?;
                        to_return.clear();
                    };
                    contract.set_storage(serialized_storage);
                    let msg = SetBorrowed {
                        key: &address.address,
                        value: &Cow::Borrowed(&contract),
                    };
                    let msg = &ServerMessage::Set(msg);
                    match context.io.write_with_fail(msg) {
                        Ok(()) => (),
                        Err(_) => {
                            context
                                .io
                                .write(&ServerMessage::Error("failed to set".to_owned()));
                            return Err(VmError::RuntimeErr("cant talk to host".to_owned()));
                        }
                    };
                    context.state.set(address.address.clone(), contract);
                    match *ops {
                        Value::List(l, _) if !l.is_empty() => {
                            let res: VMResult<Vec<Transaction>> = l
                                .into_iter()
                                .map(|trans| match trans {
                                    Value::Pair { fst, snd } => {
                                        let self_addr = self_addr.clone();

                                        let address = *fst;
                                        let contract_addr = match address {
                                            Value::String(s) => Ok(s),
                                            _ => Err(VmError::RuntimeErr(
                                                "bad transaction format from additional operations"
                                                    .to_owned(),
                                            )),
                                        }?;

                                        let content = snd.from_runtime_ticket(
                                            &mut context.ticket_table.as_ref().borrow_mut(),
                                            &mut to_return,
                                        )?;

                                        match contract_addr.starts_with("DK1") {
                                            true => {
                                                let extra =
                                                    contract_addr.split_once('%').map_or_else(
                                                        || ContractAddress {
                                                            address: contract_addr.clone(),
                                                            entrypoint: None,
                                                        },
                                                        |(address, entrypoint)| ContractAddress {
                                                            address: address.to_owned(),
                                                            entrypoint: Some(entrypoint.to_owned()),
                                                        },
                                                    );
                                                let operation = Operation::Invoke {
                                                    address: extra,
                                                    argument: FromOcamlV(content),
                                                    gas_limit: remaining_gas,
                                                };
                                                let operation = serde_json::to_string(&operation)
                                                    .map_err(|err| {
                                                    VmError::RuntimeErr(err.to_string())
                                                })?;
                                                let transaction = Transaction {
                                                    source: transaction.source.clone(),
                                                    sender: Some(self_addr.address),
                                                    operation,
                                                    operation_raw_hash: transaction
                                                        .operation_raw_hash
                                                        .clone(),
                                                    tickets: to_return.clone(),
                                                };
                                                to_return.clear();
                                                Ok(transaction)
                                            }
                                            false => {
                                                serde_json::to_string(&content).map_err(|err| {
                                                    VmError::RuntimeErr(err.to_string())
                                                })?;
                                                let operation = Operation::Transfer {
                                                    address: contract_addr,
                                                    tickets: to_return.clone(),
                                                };
                                                let operation = serde_json::to_string(&operation)
                                                    .map_err(|err| {
                                                    VmError::RuntimeErr(err.to_string())
                                                })?;
                                                let transaction = Transaction {
                                                    source: transaction.source.clone(),
                                                    sender: Some(self_addr.address),
                                                    operation,
                                                    operation_raw_hash: transaction
                                                        .operation_raw_hash
                                                        .clone(),
                                                    tickets: to_return.clone(),
                                                };
                                                to_return.clear();
                                                Ok(transaction)
                                            }
                                        }
                                    }
                                    _ => Err(VmError::RuntimeErr(
                                        "bad transaction format from additional operations"
                                            .to_owned(),
                                    )),
                                })
                                .collect();
                            let res = res?;
                            res.into_iter().try_for_each(|x| {
                                let transaction = x;
                                let new_gas =
                                    handle_transaction(context, transaction, remaining_gas)?;
                                gas_limit = new_gas;
                                Ok::<(), VmError>(())
                            })
                        }
                        _ => Ok(()),
                    }?;
                    Ok(gas_limit)
                }
                Err(x) => {
                    context.io.write(&ServerMessage::Error(x.to_string()));
                    Err(VmError::RuntimeErr("Error_ocured".to_owned()))
                }
            }
        }
        None => {
            context.io.write(&ServerMessage::Error(format!(
                "contract doesnt exist {}",
                serde_json::to_string(&address).expect("cant happen")
            )));
            Err(VmError::RuntimeErr("Error_ocured".to_owned()))
        }
    }
}
