open Base
open Core_bench
open Zinc_interpreter.Dummy
open Ir.Zt.Zinc

let convert zinc =
  let from =
    List.nth_exn zinc (List.length zinc - 1) |> snd |> Ir.Zt.Zinc.to_yojson
  in
  let to_ = Ir.Zt.Zinc.of_yojson from |> Result.ok_or_failwith in
  to_

open Z

let prog1 =
  let list =
    List.init 100000 ~f:(fun x -> [Plain_old_data (Mutez ~$x); Operation Cons])
    |> List.concat
  in
  [("a", List.concat [[Plain_old_data Nil]; list; [Core Return]])] |> convert

let prog2 =
  [
    ( "or_true",
      [
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "or_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "and_true",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "and_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "not_bool",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Core (Access 0);
        Operation Not;
        Core Return;
      ] );
  ]
  |> convert

let prog2_stack = [Ir.Zt.Stack_item.Z (Plain_old_data (Bool false))]

let prog3 =
  [
    ("a", [Plain_old_data (Num ~$2); Core Return]);
    ( "b",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Return;
      ] );
    ( "lf",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Grab;
        Plain_old_data (Bool true);
        Plain_old_data (Bool false);
        Operation Or;
        Core Grab;
        Core (Access 0);
        Adt
          (MatchVariant
             [|
               [Core Grab; Core (Access 2); Core Return];
               [Core Grab; Core (Access 3); Core Return];
             |]);
      ] );
  ]
  |> convert

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fn.id

  let key_hash s = s ^ "hash"
end

open Caml

module Mem_cell : sig
  exception Out_of_bound_write

  exception Out_of_bound_read

  type pointer = int

  type 'a t = {mutable pointer : pointer; memory : 'a array; cap : pointer}

  val pointer : 'a t -> pointer

  val set_pointer : 'a t -> value:pointer -> unit

  val read_and_increment : 'a t -> 'a

  val make : default_elem:'a -> cap:pointer -> unit -> 'a t

  val of_list : 'a list -> 'a t -> unit

  val memory : 'a t -> 'a array

  val read : 'a t -> memory:'b array -> pointer:pointer -> 'b

  val write : 'a t -> memory:'b array -> pointer:pointer -> data:'b -> unit

  val push : 'a t -> data:'a -> unit

  val pop : 'a t -> 'a

  val tail : 'a t -> unit

  val read_offset : 'a t -> offset:pointer -> 'a

  val store_closure :
    pointer t -> program_counter:pointer -> env_pointer:pointer -> pointer

  val read_closure_env : pointer t -> closure:pointer -> pointer

  val read_closure_pc : pointer t -> closure:pointer -> pointer

  val allocate : pointer t -> size:pointer -> tag:pointer -> pointer

  val read_field : 'a t -> block:pointer -> field:pointer -> 'a

  val write_field : 'a t -> block:pointer -> field:pointer -> value:'a -> unit
end = struct
  exception Out_of_bound_write

  exception Out_of_bound_read

  type pointer = int

  type 'a t = {mutable pointer : int; memory : 'a array; cap : int}

  let make ~default_elem ~cap () =
    {pointer = 0; memory = Array.make cap default_elem; cap}

  let[@inline always] pointer t = t.pointer

  let[@inline always] set_pointer t ~value = t.pointer <- value

  let[@inline always] memory t = t.memory

  let[@inline always] read_and_increment t =
    let res = Array.get t.memory t.pointer in
    t.pointer <- t.pointer + 1 ;
    res

  let[@inline always] read t ~memory ~pointer = Array.get memory pointer

  let[@inline always] write t ~memory ~pointer ~data =
    Array.set memory pointer data

  let of_list l t =
    List.iteri (fun idx x -> write t ~memory:t.memory ~pointer:idx ~data:x) l

  let[@inline always] push t ~data =
    let pointer = t.pointer in
    write t ~memory:t.memory ~pointer ~data ;
    t.pointer <- succ pointer

  let[@inline always] pop t =
    let pointer = pred t.pointer in
    let value = read t ~memory:t.memory ~pointer in
    t.pointer <- pointer ;
    value

  let[@inline always] tail ({pointer; _} as t) = t.pointer <- pred pointer

  let[@inline always] read_offset ({memory; pointer; cap = _} as t) ~offset =
    let offset = pointer - offset in
    read t ~memory ~pointer:offset

  let[@inline always] store_closure ({memory; pointer; cap = _} as t)
      ~program_counter ~env_pointer =
    let point = pointer in
    let offs = succ pointer in
    write t ~memory ~pointer:offs ~data:env_pointer ;
    let offs = succ pointer in
    write t ~memory ~pointer:offs ~data:program_counter ;
    t.pointer <- offs ;
    point

  let[@inline always] read_closure_env ({memory; pointer = _; cap = _} as t)
      ~closure =
    read t ~memory ~pointer:(closure + 0)

  let[@inline always] read_closure_pc ({memory; pointer = _; cap = _} as t)
      ~closure =
    read t ~memory ~pointer:(succ closure)

  let[@inline always] allocate t ~size ~tag =
    let point = t.pointer in
    write t ~memory:t.memory ~pointer:t.pointer ~data:size ;
    write t ~memory:t.memory ~pointer:(t.pointer + 1) ~data:tag ;
    t.pointer <- t.pointer + (size + 2) ;
    point

  let[@inline always] read_field t ~block ~field =
    let memory = t.memory in
    read t ~memory ~pointer:(block + field + 1)

  let[@inline always] write_field t ~block ~field ~value =
    let memory = t.memory in
    write t ~memory ~pointer:(block + field + 1) ~data:value
end

module type Default = sig
  type 'a t

  type pointer

  val of_list : 'a list -> 'a t -> unit

  val memory : 'a t -> 'a array

  val pointer : 'a t -> pointer

  val make : default_elem:'a -> cap:pointer -> unit -> 'a t

  val set_pointer : 'a t -> value:pointer -> unit

  val push : 'a t -> data:'a -> unit

  val pop : 'a t -> 'a

  val read : 'a t -> memory:'b array -> pointer:pointer -> 'b

  val write : 'a t -> memory:'b array -> pointer:pointer -> data:'b -> unit
end

module type Closures = sig
  type 'a t

  type pointer

  val store_closure :
    pointer t -> program_counter:pointer -> env_pointer:pointer -> pointer

  val read_closure_env : pointer t -> closure:pointer -> pointer

  val read_closure_pc : pointer t -> closure:pointer -> pointer
end

module type Read_offset = sig
  type 'a t

  type pointer

  val read_offset : 'a t -> offset:pointer -> 'a

  val tail : 'a t -> unit
end

module Stack = struct
  exception Out_of_bound_write

  exception Out_of_bound_read

  type pointer = int

  type t = {mutable pointer : pointer; memory : int array}

  let make ~default_elem ~cap () =
    {pointer = 0; memory = Array.make cap default_elem}

  let[@inline always] pointer t = t.pointer

  let[@inline always] set_pointer t ~value = t.pointer <- value

  let[@inline always] memory t = t.memory

  let[@inline always] read (t : t) ~pointer = Array.get t.memory pointer

  let[@inline always] write (t : t) ~pointer ~data =
    Array.set (t.memory : pointer array) pointer data

  let of_list l t = List.iteri (fun idx x -> write t ~pointer:idx ~data:x) l

  let[@inline always] push (t : t) ~data =
    let pointer = t.pointer in
    Array.set t.memory pointer data ;
    t.pointer <- succ pointer

  let[@inline always] pop (t : t) =
    let pointer = pred t.pointer in
    let value = Array.get t.memory pointer in
    t.pointer <- pointer ;
    value
end

module Progn : sig
  type nonrec 'a t = private 'a Mem_cell.t

  include Default with type 'a t := 'a t and type pointer := int

  val read_and_increment : 'a t -> 'a
end =
  Mem_cell

module Env : sig
  type nonrec 'a t = private 'a Mem_cell.t

  include Default with type 'a t := 'a t and type pointer := int

  include Read_offset with type 'a t := 'a t and type pointer := int
end =
  Mem_cell

module Memory = struct
  exception Out_of_bound_write

  exception Out_of_bound_read

  type pointer = int

  type t = {mutable pointer : pointer; memory : int array}

  let make ~default_elem ~cap () =
    {pointer = 0; memory = Array.make cap default_elem}

  let[@inline always] pointer t = t.pointer

  let[@inline always] set_pointer t ~value = t.pointer <- value

  let[@inline always] memory t = t.memory

  let[@inline always] read (t : t) ~(pointer : pointer) : pointer =
    Array.get t.memory pointer

  let[@inlined] write (t : t) ~pointer ~data = Array.set t.memory pointer data

  let of_list l t = List.iteri (fun idx x -> write t ~pointer:idx ~data:x) l

  let[@inline always] allocate (t : t) ~size ~tag =
    let point = t.pointer in
    Array.set t.memory point size ;
    Array.set t.memory (point + 1) tag ;
    t.pointer <- point + (size + 3) ;
    point

  let[@inline always] read_field (t : t) ~block ~field =
    Array.get t.memory (block + field + 1)

  let[@inline always] write_field (t : t) ~block ~field ~value =
    Array.set t.memory (block + field + 1) value

  let[@inline always] read_closure_env t ~closure = read t ~pointer:(closure + 0)

  let[@inline always] read_closure_pc t ~closure =
    read t ~pointer:(succ closure)
end

module T = struct
  exception Out_of_bound_write

  exception Out_of_bound_read

  exception Type_error

  (* TODO: how show we treat undefined behavior? *)
  (* TODO: use GADTs and proper fetch and decode to avoid allocations *)
  (* https://xavierleroy.org/mpri/2-4/machines.pdf SECD *)
  type instr =
    (* get nth position of env *)
    | ACCESS of int
    (* move top of the stack to env *)
    | LET
    (* drop top of the env *)
    | ENDLET
    | CLOSURE of int (* pointer to the end of the closure *)
    | APPLY
    | TAILAPPLY
    | RETURN
    (* variant that contains a value, simple variants get represented  as ints *)
    (* char array + size - bytes and strings *)
    | CONSTSTRING of string
    | CONST of int
    | BRANCHIFNOT of int (* pointer to conditional jump to branch closure *)
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
    (* value : value : a -> value : a*)
    | IADD
    | IMUL
    | IDIV
    | ISUB
    | EQINT
    | NEQINT
  [@@deriving show]

  module Table = Caml.Hashtbl.Make (struct
    type t = int

    external equal : int -> int -> bool = "%equal" [@@inline always]

    external hash : int -> int = "%identity" [@@inline always]
  end)

  (* TODO: maybe use Obj.magic in the future to remove fetch_and_decode *)

  (* TODO: this doesn't work on OCaml 32 bits*)
  type imm_32 = int

  and program = instr Progn.t

  (* TODO: maybe stack should be bigarray to avoid being scanned by GC *)
  and memory = Memory.t

  and stack = Stack.t

  and env = imm_32 Env.t

  (* TODO: should we allow JIT? *)
  and state = {
    (* 16-bits, 512kb *)
    program : program;
    stack : stack;
    env : env;
    (* TODO: benchmark how long it takes *)
    memory : memory;
    table : (int * string) Table.t;
    mutable gas_counter : int; (* TODO: proper abstract type here *)
    mutable extra_args : int;
  }

  let ptr = [0xFF; 0x00; 0x11]

  let x = (100, ptr)

  let default () =
    {
      (* 16-bits, 512kb *)
      program = Progn.make ~default_elem:RETURN ~cap:51000 ();
      stack = Stack.make ~default_elem:0 ~cap:51000 ();
      env = Env.make ~default_elem:0 ~cap:51000 ();
      (* TODO: benchmark how long it takes *)
      memory = Memory.make ~default_elem:0 ~cap:225000 ();
      table = Table.create 128;
      gas_counter = 0 (* TODO: proper abstract type here *);
      extra_args = 0;
    }

  (* TODO: adding an accu would probably be faster *)

  let verify_gas state = state.gas_counter > 0

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
    Memory.write_field
      state.memory
      ~block
      ~field:1
      ~value:(Progn.pointer state.program) ;
    Memory.write_field
      state.memory
      ~block
      ~field:2
      ~value:(Env.pointer state.env) ;
    Progn.set_pointer state.program ~value:(Progn.pointer state.program + offset) ;
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

  let[@inline always] eqint x y = if x = y then 1 else 0

  let[@inline always] neqint x y = if x = y then 0 else 1

  let rec execute state =
    let isntr = Progn.read_and_increment state.program in
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
    | CONSTSTRING b ->
        (let hashed = Base.hash_string b in
         match Table.find state.table hashed with
         | b -> Stack.push state.stack ~data:(fst b)
         | exception Not_found ->
             let block = Memory.allocate state.memory ~size:1 ~tag:252 in
             Memory.write_field state.memory ~block ~field:1 ~value:hashed ;
             Stack.push state.stack ~data:block ;
             Table.add state.table hashed (block, b)) ;
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
        if cond = 0 then
          Progn.set_pointer
            state.program
            ~value:(Progn.pointer state.program + offset)
        else () ;
        execute state
    | IADD ->
        integer_op state ( + ) ;
        execute state
    | IMUL ->
        integer_op state ( * ) ;
        execute state
    | IDIV ->
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
        execute state
end

let code =
  let open T in
  let l =
    List.init 25000 (fun _ -> 1) |> List.concat_map (fun x -> [IADD; CONST 1])
  in
  [[CONST 1; CONST 1]; l; [RETURN]] |> List.flatten

let code2 =
  T.[CLOSURE 4; ACCESS 1; CONST 1; IADD; RETURN; CONST 2; APPLY; RETURN]

(* (lambda x -> if x = 0 then 1 else 0) 2 *)
let code_branching =
  T.
    [
      CLOSURE 8;
      ACCESS 1;
      CONST 0;
      EQINT;
      BRANCHIFNOT 2;
      CONST 1;
      RETURN;
      CONST 0;
      RETURN;
      CONST 2;
      APPLY;
      RETURN;
    ]

let code_str =
  T.
    [
      CONSTSTRING "Test";
      CONSTSTRING "Test";
      CONSTSTRING "Test";
      CONSTSTRING "Test";
      CONSTSTRING "Test";
      RETURN;
    ]

let s =
  let a =
    let s = T.default () in
    Progn.of_list code s.T.program ;
    s
  in
  a

let s2 =
  let a =
    let s = T.default () in
    Progn.of_list code_branching s.T.program ;
    s
  in
  a

let runloop (s : T.state) = T.execute s

let runloop2 (s : T.state) =
  let res = T.execute s in
  res

module Counter_toy_no_alias = struct
  type instr = IADD | PUSH of int | HALT

  type instrs = instr array

  let code =
    let l = List.init 25000 (fun _ -> 1) in
    let l = List.concat_map (fun x -> [IADD; PUSH x]) l in
    Array.of_list ([PUSH 1; PUSH 1] @ l @ [HALT])

  exception Out_of_bound_write

  exception Out_of_bound_read

  let[@ocaml.inline] read (arr : int array) pointer : int =
    Array.get arr pointer

  let[@ocaml.inline] read2 (arr : instr array) pointer = Array.get arr pointer

  let[@ocaml.inline] write (arr : int array) pointer (data : int) =
    Array.set arr pointer data

  let rec interpret stack_pointer stack instr_pointer instrs =
    let instr = read2 instrs instr_pointer in
    let instr_pointer = instr_pointer + 1 in
    match instr with
    | IADD ->
        let fst = read stack stack_pointer in
        let stack_pointer = stack_pointer - 1 in
        let snd = read stack stack_pointer in
        let value = fst + snd in
        write stack stack_pointer value ;
        interpret stack_pointer stack instr_pointer instrs
    | PUSH n ->
        let stack_pointer = stack_pointer + 1 in
        write stack stack_pointer n ;
        interpret stack_pointer stack instr_pointer instrs
    | HALT -> ()
end

let tests () =
  (* let runner1 = Interpreter.eval (module Executor) in *)
  (* let runner2 = Interpreter.eval' (module Executor) ~debug:false in
     (* let prog1 = Interpreter.initial_state prog1 in
        let prog2' = Ir.of_typed prog2 in
        let prog2' = (prog2', [], Ir.of_typed_stack prog2_stack) in
        let prog2 = Interpreter.initial_state ~initial_stack:prog2_stack prog2 in
        let prog3' = Ir.of_typed prog3 in
        let prog3' = (prog3', [], []) in
        let prog3 = Interpreter.initial_state prog3 in *)
     let progn2 : Ir.t list =
       [
         Ir.Num Z.one;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Return;
       ]
     in
     let progn2 = (progn2, [], []) in *)
  (* funcall \x -> x + 1 *)
  let _ = runloop2 s2 in
  let v = Stack.pop s2.stack in
  let vall = Memory.read_field s2.memory ~block:v ~field:1 in
  Printf.printf " result is %d\n" vall ;
  Stack.set_pointer s2.T.stack ~value:0 ;
  Progn.set_pointer s2.T.program ~value:0 ;
  Env.set_pointer s2.T.env ~value:0 ;
  Memory.set_pointer s2.T.memory ~value:0 ;
  T.Table.reset s2.T.table ;
  if 0 = 0 then failwith "test" else () ;
  let test name f = Bench.Test.create f ~name in
  let stack = Array.make 51000 0 in
  [
    (* test "old_eval list_cons" (fun _ -> runner1 prog1);
       test "new_eval list_cons" (fun _ -> runner2 prog1');
       test "old_eval bools" (fun _ -> runner1 prog2);
       test "new_eval bools" (fun _ -> runner2 prog2');
       test "old_eval if_then_else" (fun _ -> runner1 prog3);
       test "new_eval if_then_else" (fun _ -> runner2 prog3'); *)
    test "array zero_alloc eval" (fun _ ->
        let _ = runloop s in
        Stack.set_pointer s.T.stack ~value:0 ;
        Progn.set_pointer s.T.program ~value:0 ;
        Memory.set_pointer s.T.memory ~value:0);
    test "array direct toy zero alloc" (fun _ ->
        Counter_toy_no_alias.interpret 0 stack 0 Counter_toy_no_alias.code);
  ]

let () = tests () |> Bench.make_command |> Core.Command.run
