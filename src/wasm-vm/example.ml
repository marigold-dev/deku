module M = Wasm
let code =
  {|(module
  (memory $mem 1)
  (export "memory" (memory $mem))
  (type $sum_t (func (param i32 i32) (result i32)))
  (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
    i32.const 0
    local.get $x
    local.get $y
    i32.add
    i32.store
    i32.const 0
    i32.load)
  (export "sum" (func $sum_f)))
|}

let () =
  let lexer = Lexing.from_string code in
  let parsed = Wasm.Parse.parse "code" lexer Wasm.Parse.Module in
  parsed |> ignore
