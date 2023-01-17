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
pub unsafe extern "C" fn malloc(size: usize) -> usize {
    let ptr = __heap_start;
    __heap_start += size;
    ptr
}

#[repr(C)]
struct StackNode {
    value: *mut u8,
    next: *mut StackNode
}

extern "C" {
    // #[no_mangle]
    static mut __michelson_stack: *mut StackNode;

    #[no_mangle]
    fn main();
}

#[no_mangle]
pub unsafe extern "C" fn michelson_push(value: *mut u8) {
    let ptr = malloc(core::mem::size_of::<StackNode>()) as *mut StackNode;
    let mut node = &mut *ptr;
    node.value = value;
    node.next = __michelson_stack;
    __michelson_stack = ptr;
}

#[no_mangle]
pub unsafe extern "C" fn michelson_drop_n(n: u32) {
    let mut node = __michelson_stack;
    let mut n = n;
    while n != 0 {
        n -= 1;
        node = (*node).next;
    }
    __michelson_stack = node;
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dup_n(n: u32) {
    let mut node = __michelson_stack;
    let mut n = n;
    while n != 0 {
        n -= 1;
        node = (*node).next;
    }
    michelson_push((*node).value);
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dug_n(n: u32) {
    let mut node = __michelson_stack;
    let head = __michelson_stack;
    let mut n = n;

    while n != 0 {
        n -= 1;
        node = (*node).next;
    }

    __michelson_stack = (*head).next;
    (*head).next = (*node).next;
    (*node).next = head;
}

#[no_mangle]
pub unsafe extern "C" fn michelson_dig_n(n: u32) {
    let mut node = __michelson_stack;
    let mut n = n;

    while n != 0 {
        n -= 1;
        node = (*node).next;
    }

    // TODO: This shouldn't use mutability
    let mut a = (*node).next;
    (*node).next = (*a).next;
    (*a).next = __michelson_stack;
    __michelson_stack = a;
}

#[no_mangle]
pub unsafe extern "C" fn _start() {
    main();
}