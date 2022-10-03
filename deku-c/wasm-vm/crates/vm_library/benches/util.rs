use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Read, Write},
    path::Path,
};

use {vm_library::vm_client::ClientMessage, vm_library::vm_server::ServerMessage};

pub struct IO {
    reader: BufReader<File>,
    writer: BufWriter<File>,
}

use fnv::FnvHashMap;
use nix::unistd;
use nix::{libc::IP_DROP_MEMBERSHIP, sys::stat::Mode};
use vm_library::{
    outgoing::{Init, SetOwned},
    vm_client::Transaction,
};
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
        source: "test".to_string(),
        sender: Some("test".to_string()),
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
                ServerMessage::Set(SetOwned { key: _, value: _ }) => continue,
                ServerMessage::Stop => break,
                _ => todo!(),
            }
        }
    }
}
pub fn invoke(operation: String) -> impl FnMut(&mut IO) {
    let t = Transaction {
        source: "test".to_string(),
        sender: Some("test".to_string()),
        operation,
        operation_raw_hash: "test".to_string(),
        tickets: vec![],
    };
    let msg =
        serde_json::to_string(&ClientMessage::Transaction(t)).expect("Failed to write to pipe");

    move |io| {
        io.write(msg.as_bytes());
        loop {
            match io.read() {
                ServerMessage::DepositTickets(_) => continue,
                ServerMessage::Set(SetOwned { key: _, value: _ }) => (),
                ServerMessage::Stop => break,
                _ => todo!(),
            }
        }
    }
}
