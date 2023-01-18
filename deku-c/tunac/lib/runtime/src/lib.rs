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

#[repr(C)]
#[derive(Clone, Copy)]
struct StackNode<'a> {
    value: *mut u8,
    next: &'a StackNode<'a>
}

impl<'a> StackNode<'a> {
    pub fn new(value: *mut u8, next: &'a StackNode<'a>) -> &'a StackNode<'a> {
        let mut node = alloc::<StackNode<'a>>();
        node.value = value;
        node.next = next;
        node
    }

    pub fn value(&'a self) -> *mut u8 {
        self.value
    }

    pub fn next(&'a self) -> &'a StackNode<'a> {
        self.next
    }

    pub fn push(&'a self, value: *mut u8) -> &'a StackNode<'a>  {
        StackNode::new(value, self)
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
pub unsafe extern "C" fn _start() {
    main();
}