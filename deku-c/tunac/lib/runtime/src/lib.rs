#![no_std]

/*
 * I'm still figuring out how to properly compile Rust to object files
 * and link them with tunac generated files, so while using only rustc
 * in order to do it we should avoid using external modules.
 */

use core::panic::PanicInfo;

#[panic_handler]
fn handle_panic(_: &PanicInfo) -> ! {
    loop {}
}


static mut __heap_start: usize = 0;

#[no_mangle]
#[inline]
pub unsafe extern "C" fn malloc(size: usize) -> usize {
    let ptr = __heap_start;
    __heap_start += size;
    ptr
}

#[inline]
fn alloc<'a, T>() -> &'a mut T {
    unsafe {
        // TODO: check pointer
        let ptr = malloc(core::mem::size_of::<T>()) as *mut T;
        &mut *ptr
    }
}

struct Box<'a, T> (&'a T);

impl<'a, T> Box<'a, T> {
    pub fn new(value: T) -> Self {
        unsafe {
            let ptr = malloc(core::mem::size_of::<T>()) as *mut T;
            core::ptr::copy(&value, ptr, 1);
            Box(&*ptr)
        }
    }

    pub fn as_ref(self) -> &'a T {
        self.0
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct StackNode<'a> {
    value: *mut u8,
    next: &'a StackNode<'a>
}

impl<'a> StackNode<'a> {
    pub fn new(value: *mut u8, next: &'a StackNode<'a>) -> Box<StackNode<'a>> {
        Box::new(StackNode { value, next })
    }

    pub fn value(&'a self) -> *mut u8 {
        self.value
    }

    pub fn next(&'a self) -> &'a StackNode<'a> {
        self.next
    }

    pub fn push(&'a self, value: *mut u8) -> &'a StackNode<'a>  {
        StackNode::new(value, self).as_ref()
    }

    pub fn drop(&'a self, n: u32) -> &'a StackNode<'a>  {
        if n == 0 {
            return self;
        }

        self.next().drop(n - 1)
    }

    pub fn dup(&'a self, n: u32) -> &'a StackNode<'a> {
        self.push(self.drop(n).value())
    }

    pub fn push_nth(&'a self, n: u32, value: *mut u8) -> &'a StackNode<'a> {
        if n == 0 {
            return self.push(value)
        }

        self.next().push_nth(n - 1, value).push(self.value())
    }

    pub fn pop_nth(&'a self, n: u32) -> (*mut u8, &'a StackNode<'a>) {
        if n == 0 {
            let digged = self.next();
            return (digged.value(), digged.next().push(self.value()))
        }

        let (value, next) = self.next().pop_nth(n - 1);
        (value, next.push(self.value()))
    }
}

extern "C" {
    static mut __michelson_stack: &'static mut StackNode<'static>;

    fn michelson_dynamic_compare(compare: u32, a: u32, b: u32) -> i32;
    fn writev(x: u32);

    fn main();
}

#[no_mangle]
pub unsafe extern "C" fn michelson_push(value: *mut u8) {
    *__michelson_stack = *__michelson_stack.push(value)
}

#[no_mangle]
pub unsafe extern "C" fn michelson_drop_n(n: u32) {
    *__michelson_stack = *__michelson_stack.drop(n);
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dup_n(n: u32) {
    *__michelson_stack = *__michelson_stack.dup(n)
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dug_n(n: u32) {
    *__michelson_stack = *__michelson_stack.push_nth(n, __michelson_stack.value())
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dig_n(n: u32) {
    let (value, stack) = __michelson_stack.pop_nth(n);
    *__michelson_stack = *stack.push(value)
}

#[no_mangle]
pub unsafe extern "C" fn michelson_map_get(map: u32, key: u32) -> u32 {
    Map::from(map).find(key).as_u32()
}

struct Value { value: u32 }

impl Value {
    pub fn from(value: u32) -> Self {
        Value { value }
    }

    pub fn as_pair(&self) -> &'static (u32, u32) {
        unsafe { &*(self.value as *const (u32, u32)) }
    }

    pub fn as_triple(&self) -> &'static (u32, u32, u32) {
        unsafe { &*(self.value as *const (u32, u32, u32)) }
    }

    pub fn as_option(&self) -> Option<u32> {
        match self.value {
            0 => None,
            _ => Some(self.as_pair().1)
        }
    }

    pub fn as_u32(&self) -> u32 {
        self.value
    }

    pub fn pair(a: u32, b: u32) -> Self {
        let pair = alloc::<(u32, u32)>();
        pair.0 = a;
        pair.1 = b;
        Value::from(pair as *const _ as u32)
    }

    pub fn triple(a: u32, b: u32, c: u32) -> Self {
        let triple = alloc::<(u32, u32, u32)>();
        triple.0 = a;
        triple.1 = b;
        triple.2 = c;
        Value::from(triple as *const _ as u32)
    }

    pub fn some(x: u32) -> Self {
        Self::pair(1, x)
    }

    pub fn none() -> Self {
        Self::from(0)
    }

    pub fn is_null(&self) -> bool {
        self.value == 0
    }
}

struct Map {
    value: Value,
    compare: u32
}

impl Map {
    pub fn insert(self, key: u32, value: u32) -> Self {
        Map {
            value: Value::triple(key, value, self.as_u32()),
            compare: self.compare
        }
    }

    pub fn from(value: u32) -> Self {
        Map {
            value: Value::from(value >> 4),
            compare: value & 0xf
        }
    }

    pub fn key(&self) -> u32 {
        self.value.as_triple().0
    }

    pub fn value(&self) -> u32 {
        self.value.as_triple().1
    }

    pub fn next(&self) -> Box<Self> {
        Box::new(Map::from(self.value.as_triple().2))
    }

    pub fn find_node(&self, key: u32) -> Option<&Map> {
        if self.value.is_null() {
            return None;
        }

        match unsafe { michelson_dynamic_compare(self.compare, key, self.key()) } {
            0 => Some(self),
            _ => self.next().as_ref().find_node(key)
        }
    }
    
    pub fn find(&self, key: u32) -> Value {
        match self.find_node(key) {
            Some(node) => Value::some(node.value()),
            None => Value::none()
        }
    }

    pub fn as_u32(&self) -> u32 {
        (self.value.as_u32() << 4) | self.compare
    }
}

#[no_mangle]
pub unsafe extern "C" fn michelson_map_update(map: u32, key: u32, value: u32) -> u32 {
    Map::from(map).insert(key, value).as_u32()
}

#[no_mangle]
pub unsafe extern "C" fn _start() {
    main();
}