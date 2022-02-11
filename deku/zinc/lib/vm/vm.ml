open Container

(*
  type t = {
    el: int;
    list: int list;
  }


  ptr + 0
  *)

(* [stuff, 123, 2, 5, 4, 6, 0] *)

(* 6 : 1 *)

(* TODO: how show we treat undefined behavior? *)
(* TODO: use GADTs and proper fetch and decode to avoid allocations *)
(* https://xavierleroy.org/mpri/2-4/machines.pdf SECD *)

(*
  instr : payload
  uint16
  *)

type instr =
  (* get nth position of env *)
  | ACCESS of int
  (* move top of the stack to env *)
  | LET
  (* drop top of the env *)
  | ENDLET
  | CLOSURE of int (* pointer to the start *)
  | APPLY
  | TAILAPPLY
  | RETURN
  (* | BRANCH of int (* offset *) *)
  (* variant that contains a value, simple variants get represented  as ints *)
  (* char array + size - bytes and strings *)
  | CONST of int
  | CONSTBYTES of bytes (* TODO: i think it will simplify stuff a lot, might remove in a few days *)
  | BRANCHIFNOT of int (* of int, offset *)
  (* pointer to conditional jump to branch closure *)
  (* size : a -> block : a *)
  (* block structure: [block_size; block_tag; value0;value1;..]
     block tags
       - 0 record, tuple
       - 1 list repr as follows: 0 - NIL, block[head; CONS Addr] we need separate tag for lists for data_encoding
       - 2 closure
       - 3 variant
       - 252 bytes
       -
  *)
  | MAKE_BLOCK
  (* field : block : a -> value : a *)
  | READ_FIELD
  (* value : field : block -> a *)
  | WRITE_FIELD
  (* value : value : a -> value : a *)
  | IADD
  | IMUL
  | IDIV
  | ISUB
  | EQINT
  | NEQINT
[@@deriving show {with_path = false}]

module Progn = Make (struct
  type t = instr
end)

(* [1, 2, 3, 0] : (int * int * int * int) -> [Int 1, Int 2, Int 3, Int 0]
   [1, 2, 3, 0] : int list -> [Cons (1, Cons (3, Nil))]

   let rec parse array ptr = (Array.get array (ptr + 0)) *)

(* TODO: maybe use Obj.magic in the future to remove fetch_and_decode *)

(* TODO: this doesn't work on OCaml 32 bits*)

type program = Progn.t

(* TODO: maybe stack should be bigarray to avoid being scanned by GC *)
type memory = Memory.t

type stack = Stack.t

type env = Env.t

(* TODO: should we allow JIT? *)
type state = {
  (* 16-bits, 512kb *)
  program : program;
  stack : stack;
  env : env;
  (* TODO: benchmark how long it takes *)
  memory : memory;
  mutable gas_counter : int; (* TODO: proper abstract type here *)
  mutable extra_args : int;
  mutable debug : bool;
}

let default () =
  {
    (* 16-bits, 512kb , ??? *)
    program = Progn.make ~default_elem:RETURN ~cap:51000 ();
    stack = Stack.make ~default_elem:(-1) ~cap:51000 ();
    env = Env.make ~default_elem:(-1) ~cap:51000 ();
    (* TODO: benchmark how long it takes *)
    memory = Memory.make ~default_elem:(-1) ~cap:225000 ();
    gas_counter = 0 (* TODO: proper abstract type here *);
    extra_args = 0;
    debug = false;
  }

(* TODO: adding an accu would probably be faster *)

(* let fetch_and_decode (state : program) =
   (* TODO: is Array.length compare + unsafe_get + raise faster? *)
   Progn.read ~memory:state.Mem_cell.memory ~pointer:state.Mem_cell.pointer *)
(* [Closure; . <- offset, ., ., ., ., ., ; Return, rest of the contract]*)
(* Closure c; access 1; const 1; add; return; const 2; apply*)
let[@inline always] integer_op state op =
  let init_a = Stack.pop state.stack in
  let a = Memory.read_field state.memory ~block:init_a ~field:1 in
  let init_b = Stack.pop state.stack in
  let b = Memory.read_field state.memory ~block:init_b ~field:1 in
  let res = Memory.allocate state.memory ~size:1 ~tag:1000 in
  let computed = op a b in
  Memory.write_field state.memory ~block:res ~field:1 ~value:computed ;
  Stack.push state.stack ~data:res

let[@inline always] alloc_int state v =
  let block = Memory.allocate state.memory ~size:1 ~tag:1000 in
  Memory.write_field state.memory ~block ~field:1 ~value:v ;
  Stack.push state.stack ~data:block

let[@inline always] alloc_clos state offset =
  let block = Memory.allocate state.memory ~size:2 ~tag:2 in
  Memory.write_field state.memory ~block ~field:1 ~value:offset ;
  Memory.write_field state.memory ~block ~field:2 ~value:(Env.pointer state.env) ;
  Stack.push state.stack ~data:block

let[@inline always] alloc_marker state =
  let block = Memory.allocate state.memory ~size:2 ~tag:2 in
  let pc_pointer = Progn.pointer state.program in
  Memory.write_field state.memory ~block ~field:1 ~value:pc_pointer ;
  let env_pointer = Env.pointer state.env in
  Memory.write_field state.memory ~block ~field:2 ~value:env_pointer ;
  Stack.push state.stack ~data:block

let[@inline always] apply_closure state closure =
  let env_pointer = Memory.read_field state.memory ~field:2 ~block:closure in
  let program_counter =
    Memory.read_field state.memory ~field:1 ~block:closure
  in
  Env.set_pointer state.env ~value:env_pointer ;
  Progn.set_pointer state.program ~value:program_counter

external bool_to_int : bool -> int = "%identity"

let[@inline always] eqint (x : int) (y : int) : int =
  bool_to_int (Int.equal x y)

let[@inline always] neqint (x : int) (y : int) : int =
  bool_to_int (not (Int.equal x y))

let[@inline always] bytes_to_int_array (buf : bytes) (size : int) dst offs :
    unit =
  for idx = 0 to size - 1 do
    let g = offs + idx in
    let i = Bytes.get buf idx in
    let code = Char.code i in
    Array.set dst g code
  done

module PP = struct
  type t =
    | Int of int
    (* | List of t
       | Pair of (t * t) *)
    | Closure of {jmp_dest : int; env_pointer : int}
    | Bytes of bytes
    | Variant of {tag : int; value : t}
  [@@deriving show {with_path = false}]

  let rec go state block =
    let tag = Memory.read state.memory ~pointer:(block + 1) in
    match tag with
    | 252 ->
        let size = Memory.read state.memory ~pointer:block in
        let sub = Array.sub (Memory.memory state.memory) (block + 3) size in
        let bytes =
          Array.map (fun x -> Char.chr x) sub |> Array.to_seq |> Bytes.of_seq
        in
        Bytes bytes
    | 1000 -> Int (Memory.read_field state.memory ~block ~field:1)
    | 3 ->
        let var = Memory.read_field state.memory ~field:1 ~block in
        let value = go state (Memory.read_field state.memory ~field:2 ~block) in
        Variant {tag = var; value}
    | 2 ->
        let jmp_dest = Memory.read_field ~block ~field:1 state.memory in
        let env_pointer = Memory.read_field ~block ~field:2 state.memory in
        Closure {jmp_dest; env_pointer}
    | 0 -> failwith "todo"
    | 1 -> failwith "todo"
    | x ->
        Printf.printf "%d\n" x ;
        failwith "todo"

  let pp_value_stack fmt state =
    let ptr = Stack.pointer state.stack + 1 in
    let to_print = Array.sub (Stack.memory state.stack) 0 ptr in
    let res =
      to_print |> Array.to_list
      |> List.concat_map (fun block ->
             if block <> -1 then [go state block] else ([] : t list))
      |> List.rev
    in
    Format.fprintf fmt "  Current stack: %s\n" ([%show: t list] res)

  let pp_value_env fmt state =
    let ptr = Env.pointer state.env + 1 in
    let to_print = Array.sub (Env.memory state.env) 0 ptr in
    let res =
      to_print |> Array.to_list
      |> List.concat_map (fun block ->
             if block <> -1 then [go state block] else ([] : t list))
      |> List.rev
    in

    Format.fprintf fmt "  Current env: %s\n" ([%show: t list] res)

  let pp_instr fmt instr =
    Format.fprintf fmt "  Current Instruction: %s\n" (show_instr instr)

  let[@inline always] print state instr =
    let template = Format.printf "Executing: \n%a%a%a" in
    template pp_instr instr pp_value_stack state pp_value_env state
end

exception Out_of_gas

(*
   TODO: memory dump and compact pointers, allocates two arrays (  momory(compacted) * stack(top element which points to the data laid out in memory)  )
*)
let extract_result state = ()

let[@inline always] intepret state ~debug ~remaining_gas ~code ~stack =
  state.gas_counter <- remaining_gas ;
  Progn.set_pointer state.program ~value:0 ;
  Stack.set_pointer state.stack ~value:0 ;
  Memory.set_pointer state.memory ~value:0 ;
  Env.set_pointer state.env ~value:0 ;
  Progn.blit code state.program ;
  Stack.blit stack state.stack ;
  state.debug <- debug ;
  let rec execute state =
    if state.gas_counter < 1 then raise Out_of_gas
    else (
      state.gas_counter <- state.gas_counter - 1 ;
      let isntr = Progn.read_and_increment state.program in
      if state.debug then PP.print state isntr ;
      match isntr with
      | ACCESS offset ->
          let value = Env.read_offset state.env ~offset in
          Stack.push state.stack ~data:value ;
          execute state
      | LET ->
          let value = Stack.pop state.stack in
          Env.push state.env ~data:value ;
          execute state
      | ENDLET ->
          Env.tail state.env ;
          execute state
      | CLOSURE offset ->
          alloc_clos state offset ;
          execute state
      | APPLY ->
          state.extra_args <- state.extra_args + 1 ;
          let value = Stack.pop state.stack in
          let closure = Stack.pop state.stack in
          alloc_marker state ;
          apply_closure state closure ;
          Env.push state.env ~data:value ;
          execute state
      | TAILAPPLY ->
          let value = Stack.pop state.stack in
          let closure = Stack.pop state.stack in
          apply_closure state closure ;
          Env.push state.env ~data:value ;
          execute state
      | RETURN ->
          if state.extra_args = 0 then ()
          else (
            state.extra_args <- state.extra_args - 1 ;
            let value = Stack.pop state.stack in
            let closure = Stack.pop state.stack in
            apply_closure state closure ;
            Stack.push state.stack ~data:value ;
            execute state)
      | CONST v ->
          alloc_int state v ;
          execute state
      | CONSTBYTES b ->
          let size = Bytes.length b in
          let block = Memory.allocate ~tag:252 ~size state.memory in
          let offs = block + 3 in
          bytes_to_int_array b size (Memory.memory state.memory) offs ;
          Stack.push state.stack ~data:block ;
          execute state
      | MAKE_BLOCK ->
          let tag = Stack.pop state.stack in
          let size = Stack.pop state.stack in
          let block = Memory.allocate state.memory ~size ~tag in
          Stack.push state.stack ~data:block ;
          execute state
      | READ_FIELD ->
          let field = Stack.pop state.stack in
          let block = Stack.pop state.stack in
          Stack.push state.stack ~data:block ;
          let value = Memory.read_field state.memory ~block ~field in
          Stack.push state.stack ~data:value ;
          execute state
      | WRITE_FIELD ->
          let value = Stack.pop state.stack in
          let field = Stack.pop state.stack in
          let block = Stack.pop state.stack in
          Stack.push state.stack ~data:block ;
          Memory.write_field state.memory ~block ~field ~value ;
          execute state
      | BRANCHIFNOT offset ->
          let block = Stack.pop state.stack in
          let cond = Memory.read_field ~block ~field:1 state.memory in
          if Int.equal cond 0 then
            Progn.set_pointer
              state.program
              ~value:(Progn.pointer state.program + offset) ;
          execute state
      | IADD ->
          integer_op state ( + ) ;
          execute state
      | IMUL ->
          integer_op state ( * ) ;
          execute state
      | IDIV ->
          (* TODO: proper exception *)
          integer_op state ( / ) ;
          execute state
      | ISUB ->
          integer_op state ( - ) ;
          execute state
      | EQINT ->
          integer_op state eqint ;
          execute state
      | NEQINT ->
          integer_op state neqint ;
          execute state)
  in
  execute state
