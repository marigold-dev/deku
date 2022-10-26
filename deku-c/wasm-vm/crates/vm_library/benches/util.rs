use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Read, Write},
    path::Path,
};

use vm_library::vm_client::ClientMessage;

pub struct IO {
    reader: BufReader<File>,
    writer: BufWriter<File>,
}
use serde::{de::Visitor, ser::SerializeTuple, Deserialize, Serialize};

use vm_library::{
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
        formatter.write_str("Expected a valid ServerMessage")
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
                    let _elem = seq.next_element::<SetOwned>()?;
                    Ok(ServerMessage::Init(InitVec(vec![])))
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

use fnv::FnvHashMap;
use nix::sys::stat::Mode;
use nix::unistd;
use vm_library::{outgoing::Init, vm_client::Transaction};
impl IO {
    pub fn new(p: String) -> Self {
        let mut consumer_mode = Mode::empty();
        consumer_mode.insert(Mode::S_IWGRP);
        consumer_mode.insert(Mode::S_IRGRP);
        consumer_mode.insert(Mode::S_IRUSR);
        consumer_mode.insert(Mode::S_IWUSR);
        consumer_mode.insert(Mode::S_IWOTH);
        consumer_mode.insert(Mode::S_IROTH);

        let mut server_mode = Mode::empty();
        server_mode.insert(Mode::S_IWGRP);
        server_mode.insert(Mode::S_IRGRP);
        server_mode.insert(Mode::S_IRUSR);
        server_mode.insert(Mode::S_IWUSR);
        server_mode.insert(Mode::S_IWOTH);
        server_mode.insert(Mode::S_IROTH);
        let mut write_path = p.clone();
        write_path.push_str("_write");
        let writer_path = Path::new(&write_path);

        unistd::mkfifo(writer_path, server_mode).unwrap_or(());
        let mut read_path = p;
        read_path.push_str("_read");
        let reader_path = Path::new(&read_path);
        unistd::mkfifo(reader_path, server_mode).unwrap_or(());

        let reader = OpenOptions::create(OpenOptions::new().read(true), false)
            .open(&read_path)
            .expect("test");

        let writer = OpenOptions::create(OpenOptions::new().write(true), false)
            .open(&write_path)
            .expect("failed to create");
        Self {
            reader: BufReader::new(reader),
            writer: BufWriter::new(writer),
        }
    }

    pub fn read(&mut self) -> ServerMessage {
        let mut len_bytes = [0u8; std::mem::size_of::<usize>()];
        self.reader
            .read_exact(&mut len_bytes)
            .expect("Bad interop format");
        let len = usize::from_ne_bytes(len_bytes);

        let mut buf = vec![0; len];
        self.reader
            .read_exact(&mut buf[..])
            .expect("Bad interop format");
        serde_json::from_slice(&buf[0..len]).expect("Bad interop format")
    }
    pub fn write(&mut self, msg: &[u8]) {
        self.writer
            .write_all(&usize::to_ne_bytes(msg.len()))
            .expect("Failed to write to pipe");
        self.writer.write_all(msg).expect("Failed to write to pipe");
        self.writer.flush().expect("Failed to write to pipe")
    }
}

pub fn init(path: String) -> IO {
    let mut io = IO::new(path);
    let msg = serde_json::to_string(&ClientMessage::SetInitialState(Init(FnvHashMap::default())))
        .expect("Failed to write to pipe");

    io.write(msg.as_bytes());
    io
}
pub fn originate(operation: String) -> impl FnMut(&mut IO) {
    let t = Transaction {
        source: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        sender: Some("tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string()),
        operation,
        operation_raw_hash: "test".to_string(),
        tickets: vec![],
    };
    let msg =
        serde_json::to_string(&ClientMessage::Transaction(t)).expect("Failed to write to pipe");
    dbg!(&msg);
    move |io| {
        io.write(msg.as_bytes());
        loop {
            match io.read() {
                ServerMessage::DepositTickets(_) => continue,
                ServerMessage::Set(SetOwned { key: _, value: _ }) => {
                    continue;
                }
                ServerMessage::Stop => break,
                ServerMessage::Error(_) => {
                    todo!()
                }
                _ => continue,
            }
        }
    }
}
pub fn invoke(operation: String) -> impl FnMut(&mut IO) {
    let t = Transaction {
        source: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        sender: Some("tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string()),
        operation,
        operation_raw_hash: "test".to_string(),
        tickets: vec![],
    };
    let msg =
        serde_json::to_string(&ClientMessage::Transaction(t)).expect("Failed to write to pipe");
    let tickets = serde_json::to_string(&ClientMessage::GiveTickets(vec![]))
        .expect("Failed to write to pipe");

    move |io| {
        io.write(msg.as_bytes());
        loop {
            match io.read() {
                ServerMessage::DepositTickets(_) => continue,
                ServerMessage::Set(SetOwned { key: _, value: _ }) => (),
                ServerMessage::Stop => break,
                ServerMessage::Error(_) => {
                    todo!()
                }
                ServerMessage::TakeTickets(_) => io.write(tickets.as_bytes()),
                _ => continue,
            }
        }
    }
}
