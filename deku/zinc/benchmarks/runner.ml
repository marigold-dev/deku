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

  val allocate : 'a t -> size:pointer -> pointer

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

  let[@inline always] read t ~memory ~pointer =
    if Int.(pointer < 0 || pointer >= t.cap) then raise Out_of_bound_read
    else Array.unsafe_get memory pointer

  let[@inline always] write t ~memory ~pointer ~data =
    if Int.(pointer < 0 || pointer >= t.cap) then raise Out_of_bound_write
    else Array.unsafe_set memory pointer data

  let of_list l t =
    List.iteri (fun idx x -> write t ~memory:t.memory ~pointer:idx ~data:x) l

  let[@inline always] push t ~data =
    let pointer = succ t.pointer in
    write t ~memory:t.memory ~pointer ~data ;
    t.pointer <- pointer

  let[@inline always] pop t =
    let pointer = pred t.pointer in
    let value = read t ~memory:t.memory ~pointer in
    t.pointer <- pointer ;
    value

  let[@inline always] tail ({memory = _; pointer; cap = _} as t) =
    t.pointer <- pred pointer

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

  let[@inline always] allocate t ~size =
    let point = t.pointer in
    t.pointer <- t.pointer + size ;
    point

  let[@inline always] read_field ({memory; pointer = _; cap = _} as t) ~block
      ~field =
    read t ~memory ~pointer:(block + field)

  let[@inline always] write_field ({memory; pointer = _; cap = _} as t) ~block
      ~field ~value =
    write t ~memory ~pointer:(block + field) ~data:value
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

module Stack : sig
  type nonrec 'a t = private 'a Mem_cell.t

  include Default with type 'a t := 'a t and type pointer := int
end =
  Mem_cell

module Progn : sig
  type nonrec 'a t = private 'a Mem_cell.t

  include Default with type 'a t := 'a t and type pointer := int
end =
  Mem_cell

module Env : sig
  type nonrec 'a t = private 'a Mem_cell.t

  include Default with type 'a t := 'a t and type pointer := int

  include Read_offset with type 'a t := 'a t and type pointer := int
end =
  Mem_cell

module Memory : sig
  type nonrec 'a t = private 'a Mem_cell.t

  type pointer = int

  include Default with type 'a t := 'a t and type pointer := pointer

  include Closures with type 'a t := 'a t and type pointer := pointer

  val allocate : 'a t -> size:pointer -> pointer

  val read_field : 'a t -> block:pointer -> field:pointer -> 'a

  val write_field : 'a t -> block:pointer -> field:pointer -> value:'a -> unit
end =
  Mem_cell

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
    | CLOSURE of int
    | APPLY
    | TAILAPPLY
    | RETURN
    | CONST of int
    (* size : a -> block : a *)
    | ALLOCATE_BLOCK
    (* field : block : a -> value : a *)
    | READ_FIELD
    (* value : field : block -> a *)
    | WRITE_FIELD
    (* value : value : a -> value : a*)
    | ADD
    | IO of (state -> unit)

  (* TODO: maybe use Obj.magic in the future to remove fetch_and_decode *)

  (* TODO: this doesn't work on OCaml 32 bits*)
  and imm_32 = int

  and program = instr Progn.t

  (* TODO: maybe stack should be bigarray to avoid being scanned by GC *)
  and memory = imm_32 Memory.t

  and stack = imm_32 Stack.t

  and env = imm_32 Env.t

  (* TODO: should we allow JIT? *)
  and state = {
    (* 16-bits, 512kb *)
    program : program;
    stack : stack;
    env : env;
    (* TODO: benchmark how long it takes *)
    memory : memory;
    mutable gas_counter : int; (* TODO: proper abstract type here *)
  }

  let default () =
    {
      (* 16-bits, 512kb *)
      program = Progn.make ~default_elem:RETURN ~cap:1000 ();
      stack = Stack.make ~default_elem:0 ~cap:1000 ();
      env = Env.make ~default_elem:0 ~cap:1000 ();
      (* TODO: benchmark how long it takes *)
      memory = Memory.make ~default_elem:0 ~cap:1000 ();
      gas_counter = 0 (* TODO: proper abstract type here *);
    }

  (* TODO: adding an accu would probably be faster *)

  let verify_gas state = state.gas_counter > 0

  (* let fetch_and_decode (state : program) =
     (* TODO: is Array.length compare + unsafe_get + raise faster? *)
     Progn.read ~memory:state.Mem_cell.memory ~pointer:state.Mem_cell.pointer *)

  (* [Closure; . <- offset, ., ., ., ., ., ; Return, rest of the contract]*)
  (* Closure c; access 1; const 1; add; return; const 2; apply*)
  let execute state instr =
    match instr with
    | IO _ -> failwith "todo"
    | ACCESS offset ->
        let value = Env.read_offset state.env ~offset in
        Stack.push state.stack ~data:value
    | LET ->
        let value = Stack.pop state.stack in
        Env.push state.env ~data:value
    | ENDLET -> Env.tail state.env
    | CLOSURE offset ->
        let closure_pointer =
          Memory.store_closure
            state.memory
            ~program_counter:(Progn.pointer state.program)
            ~env_pointer:(Env.pointer state.env)
        in
        Progn.set_pointer
          state.program
          ~value:(Progn.pointer state.program + offset) ;
        Stack.push state.stack ~data:closure_pointer
    | APPLY ->
        let value = Stack.pop state.stack in
        let closure = Stack.pop state.stack in

        Stack.push state.stack ~data:(Env.pointer state.env) ;
        Stack.push state.stack ~data:(Progn.pointer state.program) ;

        let env_pointer = Memory.read_closure_env state.memory ~closure in
        let program_counter = Memory.read_closure_pc state.memory ~closure in

        Env.set_pointer state.env ~value:env_pointer ;
        Progn.set_pointer state.program ~value:program_counter ;
        Env.push state.env ~data:value
    | TAILAPPLY ->
        let value = Stack.pop state.stack in
        let closure = Stack.pop state.stack in

        let env_pointer = Memory.read_closure_env state.memory ~closure in
        let program_counter = Memory.read_closure_pc state.memory ~closure in

        Env.set_pointer state.env ~value:env_pointer ;
        Progn.set_pointer state.program ~value:program_counter ;
        Env.push state.env ~data:value
    | RETURN ->
        let value = Stack.pop state.stack in
        let program_counter = Stack.pop state.stack in
        let env_pointer = Stack.pop state.stack in

        Stack.push state.stack ~data:value ;
        Env.set_pointer state.env ~value:env_pointer ;
        Progn.set_pointer state.program ~value:program_counter
    | CONST v -> Stack.push state.stack ~data:v
    | ALLOCATE_BLOCK ->
        let size = Stack.pop state.stack in
        let block = Memory.allocate state.memory ~size in
        Stack.push state.stack ~data:block
    | READ_FIELD ->
        let field = Stack.pop state.stack in
        let block = Stack.pop state.stack in
        Stack.push state.stack ~data:block ;
        let value = Memory.read_field state.memory ~block ~field in
        Stack.push state.stack ~data:value
    | WRITE_FIELD ->
        let value = Stack.pop state.stack in
        let field = Stack.pop state.stack in
        let block = Stack.pop state.stack in
        Stack.push state.stack ~data:block ;
        Memory.write_field state.memory ~block ~field ~value
    | ADD ->
        let a = Stack.pop state.stack in
        let b = Stack.pop state.stack in
        Stack.push state.stack ~data:(a + b)
end

let code =
  let open T in
  [CONST 1; CONST 1; ADD; RETURN]

let s =
  let a =
    let s = T.default () in
    Progn.of_list code s.program ;
    s
  in
  a

let runloop (s : T.state) =
  let idx = ref 0 in
  while !idx < 3 do
    T.execute s (Array.unsafe_get (Progn.memory s.T.program) !idx) ;
    incr idx
  done ;
  ()

let tests () =
  (* let runner1 = Interpreter.eval (module Executor) in *)
  let runner2 = Interpreter.eval' (module Executor) ~debug:false in
  (* let prog1 = Interpreter.initial_state prog1 in
     let prog2' = Ir.of_typed prog2 in
     let prog2' = (prog2', [], Ir.of_typed_stack prog2_stack) in
     let prog2 = Interpreter.initial_state ~initial_stack:prog2_stack prog2 in
     let prog3' = Ir.of_typed prog3 in
     let prog3' = (prog3', [], []) in
     let prog3 = Interpreter.initial_state prog3 in *)
  let progn2 : Ir.t list =
    [Ir.Num Z.one; Ir.Num (Z.of_int 3); Ir.Add; Ir.Return]
  in
  let progn2 = (progn2, [], []) in
  let test name f = Bench.Test.create f ~name in
  [
    (* test "old_eval list_cons" (fun _ -> runner1 prog1);
       test "new_eval list_cons" (fun _ -> runner2 prog1');
       test "old_eval bools" (fun _ -> runner1 prog2);
       test "new_eval bools" (fun _ -> runner2 prog2');
       test "old_eval if_then_else" (fun _ -> runner1 prog3);
       test "new_eval if_then_else" (fun _ -> runner2 prog3'); *)
    test "immut_eval" (fun _ -> try runner2 progn2 with _ -> ([], []));
    test "new_eval obj" (fun _ ->
        runloop s ;
        Stack.set_pointer s.T.stack ~value:0);
  ]

let () = tests () |> Bench.make_command |> Core.Command.run
