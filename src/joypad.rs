// The eight gameboy buttons/direction keys are arranged in form of a 2x4 matrix. Select either button or direction
// keys by writing to this register, then read-out bit 0-3.
//
// FF00 - P1/JOYP - Joypad (R/W)
//
// Bit 7 - Not used
// Bit 6 - Not used
// Bit 5 - P15 Select Button Keys      (0=Select)
// Bit 4 - P14 Select Direction Keys   (0=Select)
// Bit 3 - P13 Input Down  or Start    (0=Pressed) (Read Only)
// Bit 2 - P12 Input Up    or Select   (0=Pressed) (Read Only)
// Bit 1 - P11 Input Left  or Button B (0=Pressed) (Read Only)
// Bit 0 - P10 Input Right or Button A (0=Pressed) (Read Only)
//
// Note: Most programs are repeatedly reading from this port several times (the first reads used as short delay,
// allowing the inputs to stabilize, and only the value from the last read actually used).
use super::intf::{Flag, Intf};
use super::memory::Memory;
use std::cell::RefCell;
use std::rc::Rc;
use std::convert::TryFrom;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JoypadKey {
    Right  = 0b0000_0001,
    Left   = 0b0000_0010,
    Up     = 0b0000_0100,
    Down   = 0b0000_1000,
    A      = 0b0001_0000,
    B      = 0b0010_0000,
    Select = 0b0100_0000,
    Start  = 0b1000_0000,
}

pub struct Joypad {
    intf: Rc<RefCell<Intf>>,
    matrix: u8,
    select: u8,
}

impl Joypad {
    pub fn power_up(intf: Rc<RefCell<Intf>>) -> Self {
        Self {
            intf,
            matrix: 0xff,
            select: 0x00,
        }
    }
}

impl Joypad {
    pub fn keydown(&mut self, key: JoypadKey) {
        self.matrix &= !(key as u8);
        self.intf.borrow_mut().hi(Flag::Joypad);
    }

    pub fn keyup(&mut self, key: JoypadKey) {
        self.matrix |= key as u8;
    }
}

impl Memory for Joypad {
    fn get(&self, a: u16) -> u8 {
        assert_eq!(a, 0xff00);
        if (self.select & 0b0001_0000) == 0x00 {
            return self.select | (self.matrix & 0x0f);
        }
        if (self.select & 0b0010_0000) == 0x00 {
            return self.select | (self.matrix >> 4);
        }
        self.select
    }

    fn set(&mut self, a: u16, v: u8) {
        assert_eq!(a, 0xff00);
        self.select = v;
    }

    fn dump(&self) -> (usize, Vec<u64>) {
        let intf = self.intf.borrow().data;
        (3, vec![intf as u64, self.matrix as u64, self.select as u64])
    }

    fn load(&mut self, all: Vec<u64>) {
        match all.as_slice() {
            [intf, matrix, select] => {
                self.intf.borrow_mut().data = u8::try_from(intf.clone()).unwrap();
                self.matrix = u8::try_from(matrix.clone()).unwrap();
                self.select = u8::try_from(select.clone()).unwrap();
            }
            _ => panic!("Failed to load joypad state"),
        }
    }
}
