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

(* swap:
   tmp = rx;
   rx = ry;
   ry = tmp;

   dup:
   tmp = *stack;
   *(++stack) = tmp; *)

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
  (* variant that contains a value, simple variants get represented  as ints *)
  (* char array + size - bytes and strings *)
  | CONST of int
  | CONSTBYTES of bytes (* TODO: i think it will simplify stuff a lot, might remove in a few days *)
  | BRANCH of int
  | BRANCHIFNOT of int (* of int, offset *)
  | DUP
  | SWAP
  (* pointer to conditional jump to branch closure *)
  (* size : a -> block : a *)
  (* block structure: [block_size; value0;value1;..]
     irrelevant - > block tags
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
  (* Write field is useless? we only have immutable values, if we have mutable writes we need to solve issue of references to the old version(only defensive copying but that's slow ) *)
  | WRITE_FIELD (* value : value : a -> value : a *)
  (* value : pointer : a -> a *)
  | IADD
  | IMUL
  | IDIV
  | ISUB
  | EQINT
  | NEQINT

(* | CUSTOM represents foreign functions, needs a preceeding `Const <num>` tag on the stack *)
(* [@@deriving show {with_path = false}] *)

(* [1, 2, 3, 0] : (int * int * int * int) -> [Int 1, Int 2, Int 3, Int 0]
   [1, 2, 3, 0] : int list -> [Cons (1, Cons (3, Nil))]

   let rec parse array ptr = (Array.get array (ptr + 0)) *)

(* TODO: maybe use Obj.magic in the future to remove fetch_and_decode *)

(* TODO: this doesn't work on OCaml 32 bits*)
(*
   type program = Progn.t

   (* TODO: maybe stack should be bigarray to avoid being scanned by GC *)
   type memory = Memory.t

   type stack = Stack.t

   type env = Env.t *)

(* TODO: should we allow JIT? *)
type state = {
  (* 16-bits, 512kb *)
  stack : Stack.t;
  env : Env.t;
  (* TODO: benchmark how long it takes *)
  memory : Memory.t;
  mutable program_counter : int;
  mutable gas_counter : int; (* TODO: proper abstract type here *)
  mutable extra_args : int;
  mutable debug : bool;
}

let make_default () =
  {
    (* 16-bits, 512kb , ??? *)
    program_counter = 0;
    stack = Stack.make ~default_elem:(-1) ~cap:51000 ();
    env = Env.make ~default_elem:(-1) ~cap:1000 ();
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
  let a = Stack.pop state.stack in
  let b = Stack.pop state.stack in
  let computed = op a b in
  Stack.push state.stack ~data:computed

let[@inline always] alloc_clos state offset =
  let block = Memory.allocate state.memory ~size:2 in
  Memory.write_field state.memory ~block ~field:1 ~value:offset ;
  Memory.write_field state.memory ~block ~field:2 ~value:(Env.pointer state.env) ;
  Stack.push state.stack ~data:block

let[@inline always] alloc_marker state =
  let block = Memory.allocate state.memory ~size:2 in
  let pc_pointer = state.program_counter in
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
  state.program_counter <- program_counter

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

(*
   module PP = struct
     type t =
       | Int of int
       | List of t list
       | Pair of (t * t)
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
       | 0 ->
           let size = Memory.read ~pointer:block state.memory in
           let items =
             List.init size (fun x ->
                 Memory.read_field state.memory ~block ~field:x + 1)
           in
           List.fold_right
             (fun x acc -> Pair (go state x, acc))
             (List.tl items)
             (go state @@ List.hd items)
       | 1 ->
           (* lol, sorry *)
           let rec traverse state acc ptr =
             let block = Memory.read_field ~block:ptr ~field:2 state.memory in
             let tag = Memory.read state.memory ~pointer:(block + 1) in
             let hd =
               Memory.read_field ~block:ptr ~field:1 state.memory |> go state
             in
             match tag with
             | 1000 -> hd :: acc |> List.rev
             | _ -> traverse state (hd :: acc) block
           in
           List (traverse state [] block)
       | x ->
           Printf.printf "%d\n block: %d\n" x block ;
           failwith "AAAA!"

     let pp_value_stack fmt state =
       let ptr = Stack.pointer state.stack in
       let to_print = Array.sub (Stack.memory state.stack) 0 ptr in
       let res =
         to_print |> Array.to_list
         |> List.concat_map (fun block ->
                if block <> -1 then [go state block] else ([] : t list))
         |> List.rev
       in
       Format.fprintf fmt "  Current stack: %s\n" ([%show: t list] res)

     let pp_value_env fmt state =
       let ptr = Env.pointer state.env in
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

     let pp_gas fmt gas = Format.fprintf fmt "  Remaining Gas: %d\n" gas

     let[@inline always] print state instr =
       let template = Format.printf "Executing: \n%a%a%a%a" in
       template
         pp_instr
         instr
         pp_value_stack
         state
         pp_value_env
         state
         pp_gas
         state.gas_counter
   end *)

exception Out_of_gas

module Stack = Container.Stack
module Env = Container.Env
module Memory = Container.Memory
(*
   TODO: memory dump and compact pointers, allocates two arrays (  momory(compacted) * stack(top element which points to the data laid out in memory)  )
*)
(* let extract_result state out_mem out_stack = () *)

let rec execute state code =
  if state.gas_counter < 1 then raise Out_of_gas
  else (
    state.gas_counter <- state.gas_counter - 1 ;
    let isntr = Array.get code state.program_counter in
    state.program_counter <- state.program_counter + 1 ;
    let ptr = state.program_counter in
    if state.debug then () (* how do i trace if i dont have types ??? *) ;
    match isntr with
    | BRANCH dst ->
        state.program_counter <- dst ;
        execute state code
    | ACCESS offset ->
        let value = Env.read_offset state.env ~offset in

        Stack.push state.stack ~data:value ;
        execute state code
    | LET ->
        let value = Stack.pop state.stack in
        Env.push state.env ~data:value ;
        execute state code
    | ENDLET ->
        Env.tail state.env ;
        execute state code
    | CLOSURE offset ->
        alloc_clos state offset ;
        execute state code
    | APPLY ->
        state.extra_args <- state.extra_args + 1 ;
        let value = Stack.pop state.stack in
        let closure = Stack.pop state.stack in
        alloc_marker state ;
        apply_closure state closure ;
        Env.push state.env ~data:value ;
        execute state code
    | TAILAPPLY ->
        let value = Stack.pop state.stack in
        let closure = Stack.pop state.stack in
        apply_closure state closure ;
        Env.push state.env ~data:value ;
        execute state code
    | RETURN ->
        if state.extra_args = 0 then ()
        else (
          state.extra_args <- state.extra_args - 1 ;
          let value = Stack.pop state.stack in
          let closure = Stack.pop state.stack in
          apply_closure state closure ;
          Stack.push state.stack ~data:value ;
          execute state code)
    | CONST v ->
        Stack.push state.stack ~data:v ;
        execute state code
    | CONSTBYTES b ->
        let size = Bytes.length b in
        state.gas_counter <- state.gas_counter - size ;
        let block = Memory.allocate ~size state.memory in
        let offs = block + 2 in
        bytes_to_int_array b size (Memory.memory state.memory) offs ;
        Stack.push state.stack ~data:block ;
        execute state code
    (* | MAKE_BLOCK ->
        let size = Stack.pop state.stack in
        let block = Memory.allocate state.memory ~size in
        state.gas_counter <- state.gas_counter - size ;
        for idx = 1 to size do
          let value = Stack.pop state.stack in
          Memory.write_field state.memory ~field:idx ~block ~value
        done ;
        Stack.push state.stack ~data:block ;
        execute state code *)
    | MAKE_BLOCK ->
        let size = Stack.pop state.stack in
        let block = Memory.allocate state.memory ~size in
        state.gas_counter <- state.gas_counter ;
        Stack.push state.stack ~data:block ;
        execute state code
    | READ_FIELD ->
        let field = Stack.pop state.stack in
        let block = Stack.pop state.stack in
        Stack.push state.stack ~data:block ;
        let value = Memory.read_field state.memory ~block ~field in
        Stack.push state.stack ~data:value ;
        execute state code
    | WRITE_FIELD ->
        let value = Stack.pop state.stack in
        let field = Stack.pop state.stack in
        let block = Stack.pop state.stack in
        Stack.push state.stack ~data:block ;
        Memory.write_field state.memory ~block ~field ~value ;
        execute state code
    | BRANCHIFNOT offset ->
        let cond = Stack.pop state.stack in
        if Int.equal cond 0 then state.program_counter <- ptr + offset ;
        execute state code
    | IADD ->
        integer_op state ( + ) ;
        execute state code
    | IMUL ->
        integer_op state ( * ) ;
        execute state code
    | IDIV ->
        integer_op state ( / ) ;
        execute state code
    | ISUB ->
        integer_op state ( - ) ;
        execute state code
    | EQINT ->
        integer_op state eqint ;
        execute state code
    | NEQINT ->
        integer_op state neqint ;
        execute state code
    | SWAP ->
        let first = Stack.pop state.stack in
        let second = Stack.pop state.stack in
        Stack.push state.stack ~data:first ;
        Stack.push state.stack ~data:second ;
        execute state code
    | DUP ->
        let x = Stack.read state.stack ~pointer:(Stack.pointer state.stack) in
        Stack.push state.stack ~data:x ;
        execute state code)

let[@inline always] intepret ?(dry_run = false) ~debug ~remaining_gas ~code
    ~stack state =
  let _ = dry_run in
  state.gas_counter <- remaining_gas ;
  state.program_counter <- 0 ;
  Stack.set_pointer state.stack ~value:(Array.length stack) ;
  Memory.set_pointer state.memory ~value:0 ;
  Env.set_pointer state.env ~value:0 ;
  state.gas_counter <- state.gas_counter - Stack.blit stack state.stack ;
  state.debug <- debug ;
  execute state code
