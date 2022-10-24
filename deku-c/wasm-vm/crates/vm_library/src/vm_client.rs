use fnv::FnvHashMap;
use serde::{de::Visitor, ser::SerializeTuple, Deserialize, Serialize};

use crate::{
    contract_address::ContractAddress,
    managed::value::FromOcamlV,
    outgoing::{Init, SetOwned},
    path::Path,
    state::LigoCode,
    ticket_table::TicketId,
};

#[derive(Deserialize, Serialize, Debug)]
#[serde(tag = "type_", content = "content")]
pub enum Operation {
    Originate {
        module_: String,
        constants: Vec<(u32, FromOcamlV)>,
        initial_storage: FromOcamlV,
        entrypoints: Option<FnvHashMap<String, Vec<Path>>>,
        source: Option<LigoCode>,
    },
    Invoke {
        address: ContractAddress,
        argument: FromOcamlV,
        #[serde(default = "def")]
        gas_limit: u64,
    },
    Transfer {
        address: String,
        tickets: Vec<(TicketId, usize)>,
    },
}
fn def() -> u64 {
    u64::MAX
}
#[derive(Debug, Deserialize, Serialize)]
pub struct Transaction {
    pub source: String,
    #[serde(default)]
    pub sender: Option<String>,
    pub operation: String,
    pub operation_raw_hash: String,
    pub tickets: Vec<(TicketId, usize)>,
}
#[derive(Debug)]
pub enum ClientMessage {
    Transaction(Transaction),
    NoopTransaction,
    Set(SetOwned),
    GetInitialState,
    SetInitialState(Init),
    Get(ContractAddress),
    GiveTickets(Vec<(TicketId, usize)>),
}
struct ClientVisitor;
impl<'de> Visitor<'de> for ClientVisitor {
    type Value = ClientMessage;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("Expected a valid clientMessage")
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
                "Set" => {
                    let elem: Option<SetOwned> = seq.next_element()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, set"),
                                &"value",
                            ))
                        },
                        |x| Ok(ClientMessage::Set(x)),
                    )
                }
                "Give_Tickets" => {
                    let elem: Option<Vec<(TicketId, usize)>> = seq.next_element()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, give tickets"),
                                &"value",
                            ))
                        },
                        |x| Ok(ClientMessage::GiveTickets(x)),
                    )
                }
                "Get_Initial_State" => Ok(ClientMessage::GetInitialState),
                "Noop_transaction" => Ok(ClientMessage::NoopTransaction),
                "Transaction" => {
                    let elem: Option<Transaction> = seq.next_element()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, transaction"),
                                &"value",
                            ))
                        },
                        |x| Ok(ClientMessage::Transaction(x)),
                    )
                }
                "Get" => {
                    let elem: Option<ContractAddress> = seq.next_element()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str("unexpected sequence, get"),
                                &"value",
                            ))
                        },
                        |x| Ok(ClientMessage::Get(x)),
                    )
                }
                "Set_Initial_State" => {
                    let elem: Option<Init> = seq.next_element()?;
                    elem.map_or_else(
                        || {
                            Err(serde::de::Error::invalid_type(
                                serde::de::Unexpected::Str(
                                    "unexpected sequence, set initial state",
                                ),
                                &"value",
                            ))
                        },
                        |x| Ok(ClientMessage::SetInitialState(x)),
                    )
                }
                x => Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Str("unexpected sequence, dont know what to do"),
                    &x,
                )),
            },
        )
    }
}
impl<'de> Deserialize<'de> for ClientMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_tuple(2, ClientVisitor)
    }
}
impl Serialize for ClientMessage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ClientMessage::Set(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Set")?;
                seq.serialize_element(x)?;
                seq.end()
            }
            ClientMessage::Get(x) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Get")?;
                seq.serialize_element(x)?;
                seq.end()
            }
            ClientMessage::SetInitialState(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Set_Initial_State")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ClientMessage::GetInitialState => serializer.serialize_str("Get_Iinitial_State"),
            ClientMessage::GiveTickets(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Give_Tickets")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ClientMessage::Transaction(s) => {
                let mut seq = serializer.serialize_tuple(2)?;
                seq.serialize_element("Transaction")?;
                seq.serialize_element(s)?;
                seq.end()
            }
            ClientMessage::NoopTransaction => serializer.serialize_str("Noop_transaction"),
        }
    }
}
