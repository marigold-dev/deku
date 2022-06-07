    (module
    (import "env" "syscall" (func $syscall (param i64) (result i32)))
    (memory (export "memory") 1)
    (func (export "main")  (param i32) (result i64 i64 i64)
      i32.const 77
      i32.const 8
      i32.store
      i32.const 82
      i32.const -1
      i32.store
      i32.const 87
      i32.const 0
      i32.store
      i32.const 92
      i64.const 100
      i64.store
      i32.const 101
      i32.const 40
      i32.store
      i64.const 77
      call $syscall
      i64.extend_i32_s
      (i64.const 0)
      i32.const 92
      i32.const 1
      i32.store 
      i32.const 97
      i32.const 1 
      i32.store
      (i64.const 92)
      ))
