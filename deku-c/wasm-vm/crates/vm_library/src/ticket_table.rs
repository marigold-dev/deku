use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use thiserror::Error;

use crate::arena::{INVERSETICKETS, TICKETS};
#[derive(Error, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    #[error("Insuffisient funds")]
    InsufficientFunds,
    #[error("Ticket doesnt exist")]
    TicketDoesntExist,
    #[error("Ownership violation")]
    TicketOwnershipViolation,
    #[error("Split invalid amount")]
    TicketSplitInvalidAmount,
    #[error("Attempted to merge different tickets")]
    AttemptedToMergeDifferentTickets,
}

pub type Result<T> = std::result::Result<T, Error>;

type Address = String;
type Amount = usize;
type Handle = usize;

fn assert_not_dead(ticket: &Ticket) -> Result<()> {
    if ticket.live {
        Result::Ok(())
    } else {
        Result::Err(Error::TicketOwnershipViolation)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord, Hash)]
pub struct TicketId {
    pub ticketer: Address,
    pub data: String,
}

impl TicketId {
    pub fn new(contract_addr: Address, data: String) -> Self {
        TicketId {
            ticketer: contract_addr,
            data,
        }
    }
}
fn return_true() -> bool {
    true
}
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ticket {
    pub ticket_id: TicketId,
    amount: Amount,
    #[serde(skip_deserializing, default = "return_true")]
    live: bool,
}

impl Ticket {
    pub fn new(ticket_id: TicketId, amount: Amount) -> Self {
        Ticket {
            ticket_id,
            amount,
            live: true,
        }
    }

    pub fn join(ticket_1: Ticket, ticket_2: Ticket) -> Ticket {
        Ticket {
            ticket_id: ticket_1.ticket_id,
            amount: ticket_1.amount + ticket_2.amount,
            live: true,
        }
    }

    pub fn split(
        ticket_id: TicketId,
        ticket_total: Amount,
        amounts: (Amount, Amount),
    ) -> Result<(Ticket, Ticket)> {
        let total_amount = amounts.0 + amounts.1;
        if ticket_total != total_amount {
            Err(Error::TicketSplitInvalidAmount)
        } else {
            let ticket1 = Ticket {
                ticket_id: ticket_id.clone(),
                amount: amounts.0,
                live: true,
            };
            let ticket2 = Ticket {
                ticket_id,
                amount: amounts.1,
                live: true,
            };
            Ok((ticket1, ticket2))
        }
    }
}

pub struct TicketTable {
    pub counter: Handle,
    pub table: Vec<Ticket>,
}

impl Default for TicketTable {
    fn default() -> Self {
        Self {
            counter: 0,
            table: Vec::with_capacity(1000),
        }
    }
}

impl TicketTable {
    pub fn incr(&mut self) -> Handle {
        let counter = self.counter;
        self.counter += 1;
        counter
    }

    pub fn merge(&mut self, ticket: Ticket) {
        self.table.push(ticket);
    }

    pub fn unsafe_read(&mut self, handle: &Handle) -> Result<Ticket> {
        self.table.get_mut(*handle).map_or_else(
            || Err(Error::TicketDoesntExist),
            |ticket| {
                if !ticket.live {
                    return Err(Error::TicketDoesntExist);
                }
                assert_not_dead(ticket)?;
                ticket.live = false;
                Result::Ok(ticket.clone())
            },
        )
    }

    pub fn mint_ticket(&mut self, sender: Address, amount: Amount, data: String) -> Handle {
        let ticket_id = TicketId::new(sender, data);
        let ticket = Ticket::new(ticket_id, amount);
        let handle = self.incr();
        self.merge(ticket);
        handle
    }

    pub fn read_ticket(&mut self, handle: &Handle) -> Result<(TicketId, Amount, Handle)> {
        let mut ticket = self.unsafe_read(handle)?;
        let handle = self.incr();
        ticket.live = true;
        let amount = ticket.amount;
        let id = ticket.ticket_id.clone();
        self.merge(ticket);
        let to_return = (id, amount, handle);
        Result::Ok(to_return)
    }

    pub fn split_ticket(
        &mut self,
        handle: &Handle,
        amounts: (Amount, Amount),
    ) -> Result<(Handle, Handle)> {
        let ticket = self.unsafe_read(handle)?;
        let (t1, t2) = Ticket::split(ticket.ticket_id, ticket.amount, amounts)?;
        let handle_1 = self.incr();
        let handle_2 = self.incr();
        self.merge(t1);
        self.merge(t2);
        Result::Ok((handle_1, handle_2))
    }

    pub fn join_tickets(&mut self, handles: (&Handle, &Handle)) -> Result<Handle> {
        let t1 = self.unsafe_read(handles.0)?;
        let t2 = self.unsafe_read(handles.1)?;
        if t1.ticket_id == t2.ticket_id {
            let ticket = Ticket::join(t1, t2);
            let handle = self.incr();
            self.merge(ticket);
            Result::Ok(handle)
        } else {
            Result::Err(Error::TicketOwnershipViolation)
        }
    }

    pub fn finalize(&self) {
        let ticket_table = unsafe { &mut TICKETS };
        ticket_table.clear();
        self.table.iter().enumerate().for_each(|(handle, ticket)| {
            if ticket.live {
                ticket_table.insert(handle, ticket.clone());
            }
        });
    }
    pub fn populate(&mut self, tickets: &[Ticket]) {
        let ticket_table = unsafe { &mut INVERSETICKETS };
        self.table.clear();
        ticket_table.clear();
        tickets.iter().enumerate().for_each(|(handle, ticket)| {
            self.merge(ticket.clone());
            ticket_table.insert(ticket.clone(), handle);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SENDER: &str = "tz2sender";

    #[test]
    fn read_after_split_fails() {
        let mut ticket_table = TicketTable {
            counter: 0,
            table: vec![],
        };
        let handle = ticket_table.mint_ticket(SENDER.to_owned(), 10, "".to_owned());
        let _ = ticket_table.split_ticket(&handle, (4, 6)).unwrap();
        assert_eq!(
            ticket_table.read_ticket(&handle).unwrap_err(),
            Error::TicketDoesntExist
        )
    }

    #[test]
    fn read_after_join_fails() {
        let mut ticket_table = TicketTable {
            counter: 0,
            table: vec![],
        };
        let h1 = ticket_table.mint_ticket(SENDER.to_owned(), 3, "".to_owned());
        let h2 = ticket_table.mint_ticket(SENDER.to_owned(), 6, "".to_owned());

        let _ = ticket_table.join_tickets((&h1, &h2)).unwrap();

        assert_eq!(
            ticket_table.read_ticket(&h1).unwrap_err(),
            Error::TicketDoesntExist
        )
    }

    #[test]
    fn join_diff_keys_fails() {
        let mut ticket_table = TicketTable {
            counter: 0,
            table: vec![],
        };
        let h1 = ticket_table.mint_ticket(SENDER.to_owned(), 3, "1".to_owned());
        let h2 = ticket_table.mint_ticket(SENDER.to_owned(), 6, "2".to_owned());

        assert_eq!(
            ticket_table.join_tickets((&h1, &h2)).unwrap_err(),
            Error::TicketOwnershipViolation
        )
    }

    #[test]
    fn finalize_excludes_dead_tickets() {
        let mut ticket_table = TicketTable {
            counter: 0,
            table: vec![],
        };
        let h1 = ticket_table.mint_ticket(SENDER.to_owned(), 3, "".to_owned());
        let h2 = ticket_table.mint_ticket(SENDER.to_owned(), 6, "".to_owned());
        let h3 = ticket_table.join_tickets((&h1, &h2)).unwrap();

        let h4 = ticket_table.mint_ticket(SENDER.to_owned(), 12, "1".to_owned());
        let (h5, h6) = ticket_table.split_ticket(&h4, (4, 8)).unwrap();
        ticket_table.finalize();
        let final_handles = unsafe { &mut TICKETS };
        assert_eq!(
            final_handles.keys().cloned().collect::<Vec<usize>>(),
            vec![h3, h5, h6]
        )
    }
}
