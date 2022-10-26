use serde::{ser::SerializeTuple, Serialize};

use crate::{
    outgoing::{InitVec, SetBorrowed},
    ticket_table::TicketId,
};
#[derive(Serialize, Debug)]
pub struct TicketDeposit<'a> {
    pub address: &'a str,
    pub tickets: &'a [(TicketId, usize)],
}
#[derive(Debug)]
pub enum ServerMessage<'a> {
    Init(InitVec),
    Stop,
    Set(SetBorrowed<'a>),
    TakeTickets(&'a str),
    DepositTickets(TicketDeposit<'a>),
    Error(String),
}
impl<'a> Serialize for ServerMessage<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ServerMessage::Init(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Init")?;
                seq.serialize_element(x)?;
                seq.end()
            }
            ServerMessage::Stop => {
                let mut seq = serializer.serialize_tuple(1)?;
                seq.serialize_element("Stop")?;
                seq.end()
            }
            ServerMessage::Set(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Set")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ServerMessage::TakeTickets(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Take_tickets")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ServerMessage::DepositTickets(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Deposit_tickets")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ServerMessage::Error(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Error")?;
                seq.serialize_element(s)?;
                seq.end()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    struct C {
        a: i32,
        b: i32,
    }

    #[derive(Serialize, Deserialize)]
    struct Container {
        a: String,
        b: String,
    }

    #[test]
    fn can_use_bincode_with_json() {
        let c = C { a: 1, b: 2 };
        let container = Container {
            a: "Test".to_owned(),
            b: String::from_utf8_lossy(&bincode::serialize(&c).unwrap()).to_string(),
        };
        let serialized = serde_json::to_string(&container).unwrap();
        let deserialized: Container = serde_json::from_str(&serialized).unwrap();
        let c2: C = bincode::deserialize(deserialized.b.as_bytes()).unwrap();
        assert_eq!(c, c2)
    }
}
