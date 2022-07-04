              (module
              (import "env" "syscall" (func $syscall (param i64) (result i32)))
              (memory (export "memory") 1)
              (func (export "main")  (param i32) (result i64 i64 i64)
                i32.const 49
                i32.const 8
                i32.store
                i32.const 54
                i32.const -1
                i32.store
                i32.const 59
                i32.const 0
                i32.store
                i32.const 64
                i64.const 10
                i64.store
                i32.const 73
                i32.const 4
                i32.store
                i64.const 49
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
