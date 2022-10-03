use serde::{de::Visitor, ser::SerializeTuple, Deserialize, Serialize};

use crate::{
    outgoing::{InitVec, SetOwned},
    ticket_table::TicketId,
};
#[derive(Serialize, Deserialize)]
pub struct TicketDeposit {
    pub address: String,
    pub tickets: Vec<(TicketId, usize)>,
}

pub enum ServerMessage {
    Init(InitVec),
    Stop,
    Set(SetOwned),
    TakeTickets(String),
    DepositTickets(TicketDeposit),
    Error(String),
}
impl Serialize for ServerMessage {
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
struct ServerVisitor;
impl<'de> Visitor<'de> for ServerVisitor {
    type Value = ServerMessage;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Expected a valid Value")
    }
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        seq.next_element::<&str>()?.map_or_else(
            || {
                Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                ))
            },
            |x| match x {
                "Init" => {
                    let elem = seq.next_element::<InitVec>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(ServerMessage::Init(x)),
                    )
                }
                "Set" => {
                    let elem = seq.next_element::<SetOwned>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(ServerMessage::Set(x)),
                    )
                }
                "Error" => {
                    let elem = seq.next_element::<String>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(ServerMessage::Error(x)),
                    )
                }
                "Take_tickets" => {
                    let elem = seq.next_element::<String>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(ServerMessage::TakeTickets(x)),
                    )
                }
                "Deposit_tickets" => {
                    let elem = seq.next_element::<TicketDeposit>()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence"),
                                &"value",
                            ))
                        },
                        |x| Ok(ServerMessage::DepositTickets(x)),
                    )
                }
                "Stop" => Ok(ServerMessage::Stop),
                _ => Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence"),
                    &"value",
                )),
            },
        )
    }
}
impl<'de> Deserialize<'de> for ServerMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_tuple(2, ServerVisitor)
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
