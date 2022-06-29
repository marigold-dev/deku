(module
  (import "env" "syscall" (func $syscall (param i64) (result i32)))
  (memory (export "memory") 1)
  (data (i32.const 70) "\37\13")
  (data (i32.const 79) "\08")
  (data (i32.const 84) "deadbeef")

  (func $mint (param $addr i32)
    i32.const 40
    i32.const 9
    i32.store
    i32.const 45
    local.get $addr
    i32.store

    i64.const 40
    call $syscall
    drop)

  (func $own (param $addr i32)
    i32.const 40
    i32.const 5
    i32.store
    i32.const 45
    local.get $addr
    i32.store
    i64.const 40
    call $syscall
    drop)

  (func (export "main") (param i32) (result i64 i64 i64)
    i32.const 70
    call $mint

    i32.const 70
    call $own

    i64.const 70
    i64.const 40
    i64.const 120
  )
)
