// As the gameboys 16 bit address bus offers only limited space for ROM and RAM addressing, many games are using Memory
// Bank Controllers (MBCs) to expand the available address space by bank switching. These MBC chips are located in the
// game cartridge (ie. not in the gameboy itself).
//
// In each cartridge, the required (or preferred) MBC type should be specified in the byte at 0147h of the ROM, as
// described in the cartridge header. Several different MBC types are available.
//
// Reference:
//   - http://gbdev.gg8.se/wiki/articles/The_Cartridge_Header
//   - http://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers
use super::memory::Memory;
use std::convert::TryFrom;
use std::fs::File;
use std::io::Read;
use std::path::Path;

struct RealTimeClock {
    s: u8,
    m: u8,
    h: u8,
    dl: u8,
    dh: u8,
    zero: u64,
}

// HACK: I think we can make this a no-op since first-gen Pokemon didn't
// utilize the RTC at all
impl RealTimeClock {
    fn power_up() -> Self {
        // let zero = SystemTime::now()
        //     .duration_since(SystemTime::UNIX_EPOCH)
        //     .unwrap()
        //     .as_secs();
        let zero = 0;
        Self {
            zero,
            s: 0,
            m: 0,
            h: 0,
            dl: 0,
            dh: 0,
        }
    }

    fn tic(&mut self) {
        ()
        //     let d = SystemTime::now()
        //         .duration_since(SystemTime::UNIX_EPOCH)
        //         .unwrap()
        //         .as_secs()
        //         - self.zero;

        //     self.s = (d % 60) as u8;
        //     self.m = (d / 60 % 60) as u8;
        //     self.h = (d / 3600 % 24) as u8;
        //     let days = (d / 3600 / 24) as u16;
        //     self.dl = (days % 256) as u8;
        //     match days {
        //         0x0000..=0x00ff => {}
        //         0x0100..=0x01ff => {
        //             self.dh |= 0x01;
        //         }
        //         _ => {
        //             self.dh |= 0x01;
        //             self.dh |= 0x80;
        //         }
        //     }
    }
}

impl Memory for RealTimeClock {
    fn get(&self, a: u16) -> u8 {
        match a {
            0x08 => self.s,
            0x09 => self.m,
            0x0a => self.h,
            0x0b => self.dl,
            0x0c => self.dh,
            _ => panic!("No entry"),
        }
    }

    fn set(&mut self, a: u16, v: u8) {
        match a {
            0x08 => self.s = v,
            0x09 => self.m = v,
            0x0a => self.h = v,
            0x0b => self.dl = v,
            0x0c => self.dh = v,
            _ => panic!("No entry"),
        }
    }

    fn dump(&self) -> (usize, Vec<u64>) {
        (
            5,
            vec![
                self.s as u64,
                self.m as u64,
                self.h as u64,
                self.dl as u64,
                self.dh as u64,
            ],
        )
    }

    fn load(&mut self, all: Vec<u64>) {
        match all.as_slice() {
            [s, m, h, dl, dh] => {
                self.s = u8::try_from(s.clone()).unwrap();
                self.m = u8::try_from(m.clone()).unwrap();
                self.h = u8::try_from(h.clone()).unwrap();
                self.dl = u8::try_from(dl.clone()).unwrap();
                self.dh = u8::try_from(dh.clone()).unwrap();
            }
            _ => panic!("Failed to load cartridge state"),
        }
    }
}

// Beside for the ability to access up to 2MB ROM (128 banks), and 64KB RAM (8 banks), the MBC3 also includes a
// built-in Real Time Clock (RTC). The RTC requires an external 32.768 kHz Quartz Oscillator, and an external
// battery (if it should continue to tick when the gameboy is turned off).
// 0000-3FFF - ROM Bank 00 (Read Only)
// Same as for MBC1.
//
// 4000-7FFF - ROM Bank 01-7F (Read Only)
// Same as for MBC1, except that accessing banks 20h, 40h, and 60h is supported now.
//
// A000-BFFF - RAM Bank 00-03, if any (Read/Write)
// A000-BFFF - RTC Register 08-0C (Read/Write)
// Depending on the current Bank Number/RTC Register selection (see below), this memory space is used to access an
// 8KByte external RAM Bank, or a single RTC Register.
//
// 0000-1FFF - RAM and Timer Enable (Write Only)
// Mostly the same as for MBC1, a value of 0Ah will enable reading and writing to external RAM - and to the RTC
// Registers! A value of 00h will disable either.
//
// 2000-3FFF - ROM Bank Number (Write Only)
// Same as for MBC1, except that the whole 7 bits of the RAM Bank Number are written directly to this address. As for
// the MBC1, writing a value of 00h, will select Bank 01h instead. All other values 01-7Fh select the corresponding
// ROM Banks.
//
// 4000-5FFF - RAM Bank Number - or - RTC Register Select (Write Only)
// As for the MBC1s RAM Banking Mode, writing a value in range for 00h-07h maps the corresponding external RAM Bank (
// if any) into memory at A000-BFFF. When writing a value of 08h-0Ch, this will map the corresponding RTC register into
// memory at A000-BFFF. That register could then be read/written by accessing any address in that area, typically that
// is done by using address A000.
//
// 6000-7FFF - Latch Clock Data (Write Only)
// When writing 00h, and then 01h to this register, the current time becomes latched into the RTC registers. The
// latched data will not change until it becomes latched again, by repeating the write 00h->01h procedure. This is
// supposed for <reading> from the RTC registers. This can be proven by reading the latched (frozen) time from the RTC
// registers, and then unlatch the registers to show the clock itself continues to tick in background.
//
// The Clock Counter Registers
//  08h  RTC S   Seconds   0-59 (0-3Bh)
//  09h  RTC M   Minutes   0-59 (0-3Bh)
//  0Ah  RTC H   Hours     0-23 (0-17h)
//  0Bh  RTC DL  Lower 8 bits of Day Counter (0-FFh)
//  0Ch  RTC DH  Upper 1 bit of Day Counter, Carry Bit, Halt Flag
//        Bit 0  Most significant bit of Day Counter (Bit 8)
//        Bit 6  Halt (0=Active, 1=Stop Timer)
//        Bit 7  Day Counter Carry Bit (1=Counter Overflow)
// The Halt Flag is supposed to be set before <writing> to the RTC Registers.
//
// The Day Counter
// The total 9 bits of the Day Counter allow to count days in range from 0-511 (0-1FFh). The Day Counter Carry Bit
// becomes set when this value overflows. In that case the Carry Bit remains set until the program does reset it. Note
// that you can store an offset to the Day Counter in battery RAM. For example, every time you read a non-zero Day
// Counter, add this Counter to the offset in RAM, and reset the Counter to zero. This method allows to count any
// number of days, making your program Year-10000-Proof, provided that the cartridge gets used at least every 511 days.
//
// Delays
// When accessing the RTC Registers it is recommended to execute a 4ms delay (4 Cycles in Normal Speed Mode) between
// the separate accesses.
pub struct Mbc3 {
    rom: Vec<u8>,
    ram: Vec<u8>,
    rtc: RealTimeClock,
    rom_bank: usize,
    ram_bank: usize,
    ram_enable: bool,
}

impl Mbc3 {
    pub fn power_up(rom: Vec<u8>, ram: Vec<u8>) -> Self {
        Self {
            rom,
            ram,
            rtc: RealTimeClock::power_up(),
            rom_bank: 1,
            ram_bank: 0,
            ram_enable: false,
        }
    }
}

impl Memory for Mbc3 {
    fn get(&self, a: u16) -> u8 {
        match a {
            0x0000..=0x3fff => self.rom[a as usize],
            0x4000..=0x7fff => {
                let i = self.rom_bank * 0x4000 + a as usize - 0x4000;
                self.rom[i]
            }
            0xa000..=0xbfff => {
                if self.ram_enable {
                    if self.ram_bank <= 0x03 {
                        let i = self.ram_bank * 0x2000 + a as usize - 0xa000;
                        self.ram[i]
                    } else {
                        self.rtc.get(self.ram_bank as u16)
                    }
                } else {
                    0x00
                }
            }
            _ => 0x00,
        }
    }

    fn set(&mut self, a: u16, v: u8) {
        match a {
            0xa000..=0xbfff => {
                if self.ram_enable {
                    if self.ram_bank <= 0x03 {
                        let i = self.ram_bank * 0x2000 + a as usize - 0xa000;
                        self.ram[i] = v;
                    } else {
                        self.rtc.set(self.ram_bank as u16, v)
                    }
                }
            }
            0x0000..=0x1fff => {
                self.ram_enable = v & 0x0f == 0x0a;
            }
            0x2000..=0x3fff => {
                let n = (v & 0x7f) as usize;
                let n = match n {
                    0x00 => 0x01,
                    _ => n,
                };
                self.rom_bank = n;
            }
            0x4000..=0x5fff => {
                let n = (v & 0x0f) as usize;
                self.ram_bank = n;
            }
            0x6000..=0x7fff => {
                if v & 0x01 != 0 {
                    self.rtc.tic();
                }
            }
            _ => {}
        }
    }

    fn dump(&self) -> (usize, Vec<u64>) {
        let max_rom_size = 16384 * 512;
        
        let rom: Vec<u64> = self.rom.iter().map(|x| x.clone() as u64).collect();
        let rom = rom.resize(max_rom_size, 0);

        let max_ram_size = 1024 *128;
        let rom: Vec<u64> = self.ram.iter().map(|x| x.clone() as u64).collect();
        let ram = self.ram.clone().resize(max_ram_size, 0);

        let (rtc_len, rtc_bytes) = self.rtc.dump();
        
        let ram: Vec<u64> = self.ram.iter().map(|x| x.clone() as u64).collect();
        let rom_bank = self.rom_bank as u64;
        let ram_bank = self.ram_bank as u64;
        let ram_enable = u64::from(self.ram_enable);
        (
            length,
            vec![rom, ram, vec![rom_bank, ram_bank, ram_enable]].concat(),
        )
    }

    fn load(&mut self, all: Vec<u64>) {
        let rom = 
    }
}

// Specifies which Memory Bank Controller (if any) is used in the cartridge, and if further external hardware exists in
// the cartridge.
//  00h  ROM ONLY                 19h  MBC5
//  01h  MBC1                     1Ah  MBC5+RAM
//  02h  MBC1+RAM                 1Bh  MBC5+RAM+BATTERY
//  03h  MBC1+RAM+BATTERY         1Ch  MBC5+RUMBLE
//  05h  MBC2                     1Dh  MBC5+RUMBLE+RAM
//  06h  MBC2+BATTERY             1Eh  MBC5+RUMBLE+RAM+BATTERY
//  08h  ROM+RAM                  20h  MBC6
//  09h  ROM+RAM+BATTERY          22h  MBC7+SENSOR+RUMBLE+RAM+BATTERY
//  0Bh  MMM01
//  0Ch  MMM01+RAM
//  0Dh  MMM01+RAM+BATTERY
//  0Fh  MBC3+TIMER+BATTERY
//  10h  MBC3+TIMER+RAM+BATTERY   FCh  POCKET CAMERA
//  11h  MBC3                     FDh  BANDAI TAMA5
//  12h  MBC3+RAM                 FEh  HuC3
//  13h  MBC3+RAM+BATTERY         FFh  HuC1+RAM+BATTERY
pub fn power_up(path: impl AsRef<Path>) -> Box<dyn Cartridge> {
    eprintln!("Loading cartridge from {:?}", path.as_ref());
    let mut f = File::open(path.as_ref()).unwrap();
    let mut rom = Vec::new();
    f.read_to_end(&mut rom).unwrap();
    if rom.len() < 0x150 {
        panic!("Missing required information area which located at 0100-014F")
    }
    let rom_max = rom_size(rom[0x0148]);
    if rom.len() > rom_max {
        panic!("Rom size more than {}", rom_max);
    }
    let cart: Box<dyn Cartridge> = match rom[0x0147] {
        0x0f => Box::new(Mbc3::power_up(rom, vec![])),
        0x10 => {
            let ram_max = ram_size(rom[0x0149]);
            let sav_path = path.as_ref().to_path_buf().with_extension("sav");
            let ram = ram_read(sav_path.clone(), ram_max);
            Box::new(Mbc3::power_up(rom, ram))
        }
        0x11 => Box::new(Mbc3::power_up(rom, vec![])),
        0x12 => {
            let ram_max = ram_size(rom[0x0149]);
            Box::new(Mbc3::power_up(rom, vec![0; ram_max]))
        }
        0x13 => {
            let ram_max = ram_size(rom[0x0149]);
            let sav_path = path.as_ref().to_path_buf().with_extension("sav");
            let ram = ram_read(sav_path.clone(), ram_max);
            Box::new(Mbc3::power_up(rom, ram))
        }
        n => panic!("Unsupported cartridge type: 0x{:02x}", n),
    };
    eprintln!("Cartridge name is {}", cart.title());
    eprintln!("Cartridge type is {}", mbc_info(cart.get(0x0147)));
    ensure_logo(cart.as_ref());
    ensure_header_checksum(cart.as_ref());
    cart
}

// Specifies the ROM Size of the cartridge. Typically calculated as "32KB shl N".
fn rom_size(b: u8) -> usize {
    let bank = 16384;
    match b {
        0x00 => bank * 2,
        0x01 => bank * 4,
        0x02 => bank * 8,
        0x03 => bank * 16,
        0x04 => bank * 32,
        0x05 => bank * 64,
        0x06 => bank * 128,
        0x07 => bank * 256,
        0x08 => bank * 512,
        0x52 => bank * 72,
        0x53 => bank * 80,
        0x54 => bank * 96,
        n => panic!("Unsupported rom size: 0x{:02x}", n),
    }
}

// Specifies the size of the external RAM in the cartridge (if any).
fn ram_size(b: u8) -> usize {
    match b {
        0x00 => 0,
        0x01 => 1024 * 2,
        0x02 => 1024 * 8,
        0x03 => 1024 * 32,
        0x04 => 1024 * 128,
        0x05 => 1024 * 64,
        n => panic!("Unsupported ram size: 0x{:02x}", n),
    }
}

// Specifies the size of the external RAM in the cartridge (if any).
fn ram_read(path: impl AsRef<Path>, size: usize) -> Vec<u8> {
    match File::open(path) {
        Ok(mut ok) => {
            let mut ram = Vec::new();
            ok.read_to_end(&mut ram).unwrap();
            ram
        }
        Err(_) => vec![0; size],
    }
}

// Readable form of MBC representation
fn mbc_info(b: u8) -> String {
    String::from(match b {
        0x00 => "ROM ONLY",
        0x01 => "MBC1",
        0x02 => "MBC1+RAM",
        0x03 => "MBC1+RAM+BATTERY",
        0x05 => "MBC2",
        0x06 => "MBC2+BATTERY",
        0x08 => "ROM+RAM",
        0x09 => "ROM+RAM+BATTERY",
        0x0b => "MMM01",
        0x0c => "MMM01+RAM",
        0x0d => "MMM01+RAM+BATTERY",
        0x0f => "MBC3+TIMER+BATTERY",
        0x10 => "MBC3+TIMER+RAM+BATTERY",
        0x11 => "MBC3",
        0x12 => "MBC3+RAM",
        0x13 => "MBC3+RAM+BATTERY",
        0x15 => "MBC4",
        0x16 => "MBC4+RAM",
        0x17 => "MBC4+RAM+BATTERY",
        0x19 => "MBC5",
        0x1a => "MBC5+RAM",
        0x1b => "MBC5+RAM+BATTERY",
        0x1c => "MBC5+RUMBLE",
        0x1d => "MBC5+RUMBLE+RAM",
        0x1e => "MBC5+RUMBLE+RAM+BATTERY",
        0xfc => "POCKET CAMERA",
        0xfd => "BANDAI TAMA5",
        0xfe => "HuC3",
        0x1f => "HuC1+RAM+BATTERY",
        n => panic!("Unsupported cartridge type: 0x{:02x}", n),
    })
}

// These bytes define the bitmap of the Nintendo logo that is displayed when the gameboy gets turned on.
// The reason for joining is because if the pirates copy the cartridge, they must also copy Nintendo's LOGO,
// which infringes the trademark law. In the early days, the copyright law is not perfect for the determination of
// electronic data.
// The hexdump of this bitmap is:
const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

// Ensure Nintendo Logo.
fn ensure_logo(cart: &dyn Cartridge) {
    for i in 0..48 {
        if cart.get(0x0104 + i as u16) != NINTENDO_LOGO[i as usize] {
            panic!("Nintendo logo is incorrect")
        }
    }
}

// In position 0x14d, contains an 8 bit checksum across the cartridge header bytes 0134-014C. The checksum is
// calculated as follows:
//
//   x=0:FOR i=0134h TO 014Ch:x=x-MEM[i]-1:NEXT
//
// The lower 8 bits of the result must be the same than the value in this entry. The GAME WON'T WORK if this
// checksum is incorrect.
fn ensure_header_checksum(cart: &dyn Cartridge) {
    let mut v: u8 = 0;
    for i in 0x0134..0x014d {
        v = v.wrapping_sub(cart.get(i)).wrapping_sub(1);
    }
    if cart.get(0x014d) != v {
        panic!("Cartridge's header checksum is incorrect")
    }
}

pub trait Cartridge: Memory + Send {
    // Title of the game in UPPER CASE ASCII. If it is less than 16 characters then the remaining bytes are filled with
    // 00's. When inventing the CGB, Nintendo has reduced the length of this area to 15 characters, and some months
    // later they had the fantastic idea to reduce it to 11 characters only. The new meaning of the ex-title bytes is
    // described below.
    fn title(&self) -> String {
        let mut buf = String::new();
        let ic = 0x0134;
        let oc = if self.get(0x0143) == 0x80 {
            0x013e
        } else {
            0x0143
        };
        for i in ic..oc {
            match self.get(i) {
                0 => break,
                v => buf.push(v as char),
            }
        }
        buf
    }
}

impl Cartridge for Mbc3 {}
