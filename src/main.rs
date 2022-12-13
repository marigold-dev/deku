use core::time;
use std::fs::{File, OpenOptions};
use std::io::{prelude::*, BufReader};
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;
// Note: Game BoyTM, Game Boy PocketTM, Super Game BoyTM and Game Boy ColorTM are registered trademarks of
// Nintendo CO., LTD. © 1989 to 1999 by Nintendo CO., LTD.

use gameboy::gpu::{SCREEN_H, SCREEN_W};
use gameboy::joypad;
use gameboy::motherboard::MotherBoard;
use std::io::{self, BufRead};
use std::path::Path;

#[cfg(not(feature = "audio"))]
fn initialize_audio(_: &gameboy::motherboard::MotherBoard) {
    panic!("audio is not supported");
}

#[cfg(feature = "audio")]
fn initialize_audio(mbrd: &gameboy::motherboard::MotherBoard) {
    use gameboy::apu::Apu;
    let device = cpal::default_output_device().unwrap();
    let format = device.default_output_format().unwrap();
    eprintln!(
        "Open the audio player: {}. Sample rate: {:?}",
        device.name(),
        format.sample_rate.0
    );
    let format = cpal::Format {
        channels: 2,
        sample_rate: format.sample_rate,
        data_type: cpal::SampleFormat::F32,
    };

    let event_loop = cpal::EventLoop::new();
    let stream_id = event_loop.build_output_stream(&device, &format).unwrap();
    event_loop.play_stream(stream_id);

    let apu = Apu::power_up(format.sample_rate.0);
    let apu_data = apu.buffer.clone();
    mbrd.mmu.borrow_mut().apu = Some(apu);

    std::thread::spawn(move || {
        event_loop.run(move |_, stream_data| {
            let mut apu_data = apu_data.lock().unwrap();
            if let cpal::StreamData::Output { buffer } = stream_data {
                let len = std::cmp::min(buffer.len() / 2, apu_data.len());
                match buffer {
                    cpal::UnknownTypeOutputBuffer::F32(mut buffer) => {
                        for (i, (data_l, data_r)) in apu_data.drain(..len).enumerate() {
                            buffer[i * 2] = data_l;
                            buffer[i * 2 + 1] = data_r;
                        }
                    }
                    cpal::UnknownTypeOutputBuffer::U16(mut buffer) => {
                        for (i, (data_l, data_r)) in apu_data.drain(..len).enumerate() {
                            buffer[i * 2] = (data_l * f32::from(std::i16::MAX)
                                + f32::from(std::u16::MAX) / 2.0)
                                as u16;
                            buffer[i * 2 + 1] = (data_r * f32::from(std::i16::MAX)
                                + f32::from(std::u16::MAX) / 2.0)
                                as u16;
                        }
                    }
                    cpal::UnknownTypeOutputBuffer::I16(mut buffer) => {
                        for (i, (data_l, data_r)) in apu_data.drain(..len).enumerate() {
                            buffer[i * 2] = (data_l * f32::from(std::i16::MAX)) as i16;
                            buffer[i * 2 + 1] = (data_r * f32::from(std::i16::MAX)) as i16;
                        }
                    }
                }
            }
        });
    });
}

// For playing the game with keyboard input
fn main_type0(mut mbrd: MotherBoard, c_audio: bool) {
    // Initialize audio related
    if c_audio {
        initialize_audio(&mbrd);
    }

    let mut option = minifb::WindowOptions::default();
    option.resize = false;
    let rom_name = mbrd.mmu.borrow().cartridge.title();
    let mut window = minifb::Window::new(
        format!("Gameboy - {}", rom_name).as_str(),
        SCREEN_W,
        SCREEN_H,
        option,
    )
    .unwrap();
    let mut window_buffer = vec![0x00; SCREEN_W * SCREEN_H];
    window.update_with_buffer(window_buffer.as_slice()).unwrap();

    loop {
        // Stop the program, if the GUI is closed by the user
        if !window.is_open() {
            break;
        }

        // Execute an instruction
        mbrd.next();

        // Update the window
        if mbrd.check_and_reset_gpu_updated() {
            let mut i: usize = 0;
            for l in mbrd.mmu.borrow().gpu.data.iter() {
                for w in l.iter() {
                    let b = u32::from(w[0]) << 16;
                    let g = u32::from(w[1]) << 8;
                    let r = u32::from(w[2]);
                    let a = 0xff00_0000;

                    window_buffer[i] = a | b | g | r;
                    i += 1;
                }
            }
            window.update_with_buffer(window_buffer.as_slice()).unwrap();
        }

        if !mbrd.cpu.flip() {
            continue;
        }

        // Handle keyboard events
        if window.is_key_down(minifb::Key::Escape) {
            break;
        }
        let keys = vec![
            (minifb::Key::Right, gameboy::joypad::JoypadKey::Right),
            (minifb::Key::Up, gameboy::joypad::JoypadKey::Up),
            (minifb::Key::Left, gameboy::joypad::JoypadKey::Left),
            (minifb::Key::Down, gameboy::joypad::JoypadKey::Down),
            (minifb::Key::Z, gameboy::joypad::JoypadKey::A),
            (minifb::Key::X, gameboy::joypad::JoypadKey::B),
            (minifb::Key::Space, gameboy::joypad::JoypadKey::Select),
            (minifb::Key::Enter, gameboy::joypad::JoypadKey::Start),
        ];
        for (rk, vk) in &keys {
            if window.is_key_down(*rk) {
                mbrd.mmu.borrow_mut().joypad.keydown(vk.clone());
            } else {
                mbrd.mmu.borrow_mut().joypad.keyup(vk.clone());
            }
        }
    }
}

#[derive(Debug)]
enum Command {
    Input(Option<joypad::JoypadKey>),
    InputAndAdvance((Option<joypad::JoypadKey>, u32)),
}

#[derive(Debug)]
enum CommandError {
    FailedToParseMessage(),
}

#[derive(Debug)]
enum Response {
    State(u128),
}

fn response_to_string(response: Response) -> String {
    match response {
        Response::State(n) => n.to_string(),
    }
}

fn joypad_input_of_string(buffer: &str) -> Option<joypad::JoypadKey> {
    match &buffer[..] {
        "A" => Some(joypad::JoypadKey::A),
        "B" => Some(joypad::JoypadKey::B),
        "Start" => Some(joypad::JoypadKey::Start),
        "Select" => Some(joypad::JoypadKey::Select),
        "Up" => Some(joypad::JoypadKey::Up),
        "Down" => Some(joypad::JoypadKey::Down),
        "Left" => Some(joypad::JoypadKey::Left),
        "Right" => Some(joypad::JoypadKey::Right),
        _ => None,
    }
}

fn command_of_string(buffer: String) -> Option<Command> {
    let tokens: Vec<&str> = buffer.split(" ").collect();
    match tokens.get(0) {
        Some(&"Input") => {
            let joypad = joypad_input_of_string(tokens.get(1)?);
            Some(Command::Input(joypad))
        }
        Some(&"Input_and_advance") => {
            let joypad = joypad_input_of_string(tokens.get(1)?);
            let advance = tokens.get(2)?;
            let advance: Result<u32, _> = advance.parse();
            match advance {
                Ok(advance) => Some(Command::InputAndAdvance((joypad, advance))),
                Err(_) => None,
            }
        }
        Some(_) | None => None,
    }
}

fn main_test() {}

fn setup_channels(port: u16) -> (Receiver<Command>, Sender<Response>) {
    let (server_tx, server_rx) = mpsc::channel::<Command>();
    let (emulator_tx, emulator_rx) = mpsc::channel::<Response>();
    thread::spawn(move || loop {
        let address = "127.0.0.1:".to_string() + port.to_string().as_str();
        let listener = TcpListener::bind(address).unwrap();

        for stream in listener.incoming() {
            let mut stream = stream.unwrap();
            eprintln!("Got connection to {:?}", stream.local_addr());
            let mut buf_reader = BufReader::new(&mut stream);
            let mut buffer = String::new();
            buf_reader.read_line(&mut buffer).unwrap();
            eprintln!("Got request '{}'", buffer.trim());

            match command_of_string(buffer.trim().to_string()) {
                Some(command) => {
                    server_tx.send(command).unwrap();
                    let response = emulator_rx.recv().unwrap();
                    let response = response_to_string(response);
                    stream.write((response + "\n").as_bytes()).unwrap();
                    stream.flush().unwrap();
                }
                None => eprintln!("Error: unable to parse message {}", buffer),
            }
        }
    });
    (server_rx, emulator_tx)
}

// measured in cycles
const INPUT_DURATION: u32 = 80000;

// For playing the game on-chain
fn main_type1(mut _mbrd: MotherBoard) {
    panic!("unfinished!");
    // let mut option = minifb::WindowOptions::default();
    // option.resize = false;
    // let rom_name = mbrd.mmu.borrow().cartridge.title();
    // let mut window =
    //     minifb::Window::new(format!("Gameboy - {}", rom_name).as_str(), SCREEN_W, SCREEN_H, option).unwrap();
    // let mut window_buffer = vec![0x00; SCREEN_W * SCREEN_H];
    // window.update_with_buffer(window_buffer.as_slice()).unwrap();

    // let mut cycles: u32 = 0;
    // let mut key_down_cycles: u32 = 0;
    // let mut key_down: Option<(u32, joypad::JoypadKey)> = None;
    // loop {
    //     cycles += 1;
    //     key_down_cycles += 1;
    //     let mut buffer = String::new();
    //     io::stdin().read_line(&mut buffer).unwrap();
    //     match command_of_string(buffer) {
    //         Command::Input(key) => (),
    //         _ => ()
    //     }

    //     // Execute an instruction
    //     mbrd.next();

    //     // Update the window
    //     mbrd.check_and_reset_gpu_updated();

    //     if !mbrd.cpu.flip() {
    //         continue;
    //     }

    //     // Handle keyboard events
    //     if window.is_key_down(minifb::Key::Escape) {
    //         break;
    //     }
    //     let keys = vec![
    //         (minifb::Key::Right, gameboy::joypad::JoypadKey::Right),
    //         (minifb::Key::Up, gameboy::joypad::JoypadKey::Up),
    //         (minifb::Key::Left, gameboy::joypad::JoypadKey::Left),
    //         (minifb::Key::Down, gameboy::joypad::JoypadKey::Down),
    //         (minifb::Key::Z, gameboy::joypad::JoypadKey::A),
    //         (minifb::Key::X, gameboy::joypad::JoypadKey::B),
    //         (minifb::Key::Space, gameboy::joypad::JoypadKey::Select),
    //         (minifb::Key::Enter, gameboy::joypad::JoypadKey::Start),
    //     ];
    //     for (rk, vk) in &keys {
    //         match key_down {
    //             Some((last_cycle, key)) if key.eq(vk) && key_down_cycles > last_cycle + INPUT_DURATION => {
    //                 println!("Key up: {:?}", key);
    //                 mbrd.mmu.borrow_mut().joypad.keyup(vk.clone());
    //                 key_down = None;
    //                 key_down_cycles = 0;
    //             }
    //             Some((_, key)) if key.eq(vk) => (),
    //             None if window.is_key_down(*rk) => {
    //                 println!("Key down at {} cycles", key_down_cycles);
    //                 key_down = Some((key_down_cycles.clone(), vk.clone()));
    //                 mbrd.mmu.borrow_mut().joypad.keydown(vk.clone());
    //             }
    //             Some(_) | None => mbrd.mmu.borrow_mut().joypad.keyup(vk.clone()),
    //         };
    //     }
    // }
}

fn press_key(mbrd: &MotherBoard, input_key: Option<joypad::JoypadKey>) {
    let keys = vec![
        (gameboy::joypad::JoypadKey::Right),
        (gameboy::joypad::JoypadKey::Up),
        (gameboy::joypad::JoypadKey::Left),
        (gameboy::joypad::JoypadKey::Down),
        (gameboy::joypad::JoypadKey::A),
        (gameboy::joypad::JoypadKey::B),
        (gameboy::joypad::JoypadKey::Select),
        (gameboy::joypad::JoypadKey::Start),
    ];
    for key in keys {
        match input_key {
            Some(input_key) if key.eq(&input_key) => {
                mbrd.mmu.borrow_mut().joypad.keydown(key.clone())
            }
            Some(_) | None => mbrd.mmu.borrow_mut().joypad.keyup(key.clone()),
        }
    }
}

// For playing the game as the block producer
fn main_type2(mut mbrd: MotherBoard, c_audio: bool) {
    let (in_channel, out_channel) = setup_channels(2222);

    // Initialize audio related
    if c_audio {
        initialize_audio(&mbrd);
    }

    let mut option = minifb::WindowOptions::default();
    option.resize = false;
    let rom_name = mbrd.mmu.borrow().cartridge.title();
    let mut window = minifb::Window::new(
        format!("Gameboy - {}", rom_name).as_str(),
        SCREEN_W,
        SCREEN_H,
        option,
    )
    .unwrap();
    let mut window_buffer = vec![0x00; SCREEN_W * SCREEN_H];
    window.update_with_buffer(window_buffer.as_slice()).unwrap();

    let mut cycles: u128 = 0;
    let mut key_down: Option<(u32, joypad::JoypadKey)> = None;
    loop {
        cycles += 1;

        // Stop the program, if the GUI is closed by the user
        if !window.is_open() {
            break;
        }

        // Execute an instruction
        mbrd.next();

        // Update the window
        if mbrd.check_and_reset_gpu_updated() {
            let mut i: usize = 0;
            for l in mbrd.mmu.borrow().gpu.data.iter() {
                for w in l.iter() {
                    let b = u32::from(w[0]) << 16;
                    let g = u32::from(w[1]) << 8;
                    let r = u32::from(w[2]);
                    let a = 0xff00_0000;

                    window_buffer[i] = a | b | g | r;
                    i += 1;
                }
            }
            window.update_with_buffer(window_buffer.as_slice()).unwrap();
        }

        match key_down {
            Some((last_cycle, key)) => {
                key_down = Some((last_cycle + 1, key));
            }
            None =>
            // Handle incoming commands
            // TODO: the point of putting it in this match is
            // so that inputs resolve before we take on new commands.
            // not sure if this works.
            {
                match in_channel.try_recv() {
                    Ok(Command::Input(key)) => {
                        match key {
                            Some(key) => {
                                eprintln!("Received Input {:?}", key);
                                key_down = Some((0, key))
                            }
                            None => (),
                        }
                        out_channel.send(Response::State(cycles)).unwrap();
                    }
                    Ok(Command::InputAndAdvance(_)) => {
                        panic!("Received Input_and_advance, but that command is not supported in type 2")
                    }
                    Err(TryRecvError::Empty) => (),
                    Err(TryRecvError::Disconnected) => panic!("Thread channel disconnected"),
                }
            }
        };

        if !mbrd.cpu.flip() {
            continue;
        }

        // Handle keyboard events
        if window.is_key_down(minifb::Key::Escape) {
            break;
        }

        match key_down {
            Some((last_cycle, key)) => {
                if last_cycle > INPUT_DURATION {
                    println!("Releasing key {:?}", key);
                    press_key(&mbrd, None);
                    key_down = None
                } else {
                    press_key(&mbrd, Some(key));
                }
            }
            None => {
                press_key(&mbrd, None);
            }
        }
    }
}

#[cfg(feature = "gui")]
fn main() {
    let mut rom = String::from("");
    let mut c_audio = false;
    let mut runtime_type = 0;
    {
        let mut ap = argparse::ArgumentParser::new();
        ap.set_description("Gameboy emulator");
        ap.refer(&mut c_audio).add_option(
            &["-a", "--enable-audio"],
            argparse::StoreTrue,
            "Enable audio",
        );
        ap.refer(&mut runtime_type).add_option(
            &["-t", "--runtime-type"],
            argparse::Store,
            "Runtime type",
        );
        ap.refer(&mut rom)
            .add_argument("rom", argparse::Store, "Rom name");

        ap.parse_args_or_exit();
    }

    eprintln!("Running in runtime type {}", runtime_type);

    let mbrd = MotherBoard::power_up(rom);

    match runtime_type {
        0 => main_type0(mbrd, c_audio),
        1 => main_type1(mbrd),
        2 => main_type2(mbrd, c_audio),
        3 => main_test(),
        _ => panic!("You must choose type 1 or type 2"),
    }
}
