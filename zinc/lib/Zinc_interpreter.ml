open Zinc_utils
open! Zinc_types

(* they have the same constructors *)
external env_to_stack : Env_item.t -> Stack_item.t = "%identity"

external stack_to_env_ext : Stack_item.t -> Env_item.t = "%identity"

let stack_to_env = function
  | Stack_item.Marker _ ->
      failwith "type error, cant convert a stack_item into an env_item"
  | Stack_item.(Clos _ | Record _ | Variant _ | Z _ | List _) as x ->
      stack_to_env_ext x

type steps =
  | Done
  | Internal_error of string
  | Failwith of string
  | Continue of zinc_extended * env * stack

let initial_state ?initial_stack:(stack = []) a = (a, [], stack)

let[@warning "-4"] interpret_zinc :
    interpreter_context -> interpreter_input -> Interpreter_output.t =
 fun interpreter_context (code, env, stack) ->
  let apply_once (code : zinc_extended) (env : env) (stack : stack) =
    let () =
      print_endline
        (Format.asprintf
           "interpreting:\ncode:  %a\nenv:   %a\nstack: %a"
           pp_zinc_extended
           code
           pp_env
           env
           pp_stack
           stack)
    in
    match (code, env, stack) with
    | (Nil :: c, env, s) -> Continue (c, env, Stack_item.List [] :: s)
    | (Cons :: c, env, item :: Stack_item.List x :: s) ->
        Continue (c, env, Stack_item.List (item :: x) :: s)
    | (Grab :: c, env, Stack_item.Marker (c', e') :: s) ->
        Continue (c', e', Stack_item.Clos {Clos.code = Grab :: c; env} :: s)
    | (Grab :: c, env, v :: s) -> Continue (c, stack_to_env v :: env, s)
    | (Grab :: _, _, []) -> failwith "nothing to grab!"
    | (Return :: _, _, Stack_item.Z v :: Stack_item.Marker (c', e') :: s) ->
        Continue (c', e', Stack_item.Z v :: s)
    | (Return :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s) ->
        Continue (c', e', s)
    | (PushRetAddr c' :: c, env, s) ->
        Continue (c, env, Stack_item.Marker (c', env) :: s)
    | (Apply :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s) ->
        Continue (c', e', s)
    (* Below here is just modern SECD *)
    | (Access n :: c, env, s) -> (
        let nth = Base.List.nth env n in
        match nth with
        | Some nth -> Continue (c, env, (nth |> env_to_stack) :: s)
        | None -> Internal_error "Tried to access env item out of bounds")
    | (Closure c' :: c, env, s) ->
        Continue (c, env, Stack_item.Clos {Clos.code = c'; env} :: s)
    | (EndLet :: c, _ :: env, s) -> Continue (c, env, s)
    (* zinc extensions *)
    (* operations that jsut drop something on the stack haha *)
    | ( ((Num _ | Address _ | Key _ | Hash _ | Bool _ | String _ | Mutez _) as v)
        :: c,
        env,
        s ) ->
        Continue (c, env, Stack_item.Z v :: s)
    (* ADTs *)
    | (MakeRecord r :: c, env, s) ->
        let list_split_at ~n lst =
          let rec go n acc = function
            | [] ->
                if Int.equal n 0 then (acc, [])
                else raise (Invalid_argument "not enough entries on the list")
            | x when Int.equal n 0 -> (acc, x)
            | x :: xs -> go (n - 1) (x :: acc) xs
          in
          go n [] lst
        in
        let (record, stack) = list_split_at ~n:r s in
        let record_contents = LMap.of_list (List.rev record) in
        Continue (c, env, Stack_item.Record record_contents :: stack)
    | (RecordAccess accessor :: c, env, Stack_item.Record r :: s) ->
        let res =
          match LMap.find r accessor with
          | None -> failwith "field not found"
          | Some x -> Continue (c, env, x :: s)
        in
        res
    | (MatchVariant vs :: c, env, Stack_item.Variant (label, item) :: s) -> (
        match
          Base.List.find_map vs ~f:(fun (match_arm, constructors) ->
              if String.equal match_arm label then Some constructors else None)
        with
        | None -> Internal_error "inexhaustive match"
        | Some match_code ->
            Continue (List.concat [match_code; c], env, item :: s))
    (* Math *)
    | (Add :: c, env, Stack_item.Z (Num a) :: Stack_item.Z (Num b) :: s) ->
        Continue (c, env, Stack_item.Z (Num (Z.add a b)) :: s)
    | (Add :: c, env, Stack_item.Z (Mutez a) :: Stack_item.Z (Mutez b) :: s) ->
        Continue (c, env, Stack_item.Z (Mutez (Z.add a b)) :: s)
    (* Booleans *)
    | (Eq :: c, env, a :: b :: s) ->
        Continue (c, env, Stack_item.Z (Bool (Stack_item.equal a b)) :: s)
    (* Crypto *)
    | (HashKey :: c, env, Stack_item.Z (Key _key) :: s) ->
        let h = failwith "need to move this into interpreter_context" in
        Continue (c, env, Stack_item.Z (Hash h) :: s)
    (* Tezos specific *)
    | (ChainID :: c, env, s) ->
        Continue
          ( c,
            env,
            Stack_item.Z
              (* TODO: fix this usage of Digestif.BLAKE2B.hmac_string - should use an effect system or smth.
                 Also probably shouldn't use key like this. *)
              (let h = failwith "need to move this into interpreter_context" in
               Hash h)
            :: s )
    | (Contract_opt :: c, env, Stack_item.Z (Address address) :: s) ->
        (* todo: abstract this into a function *)
        let contract =
          match interpreter_context.get_contract_opt address with
          | Some (address, entrypoint) ->
              Stack_item.Variant
                ( "Some",
                  Stack_item.Z (Extensions (Contract (address, entrypoint))) )
          | None -> Stack_item.Variant ("None", Utils.unit_record_stack)
        in
        Continue (c, env, contract :: s)
    | ( MakeTransaction :: c,
        env,
        r
        :: Stack_item.Z (Mutez amount)
           :: Stack_item.Z (Extensions (Contract contract)) :: s )
      when Stack_item.equal r Utils.unit_record_stack ->
        Continue
          ( c,
            env,
            Stack_item.Z
              (Extensions (Operation (Transaction (amount, contract))))
            :: s )
    (* should be unreachable except when program is done *)
    | ([Return], _, _) -> Done
    | (Failwith :: _, _, Stack_item.Z (String s) :: _) -> Failwith s
    (* should not be reachable *)
    | (x :: _, _, _) ->
        Internal_error
          (Format.asprintf "%a unimplemented!" pp_zinc_instruction_extended x)
    | _ ->
        Internal_error
          (Format.asprintf "somehow ran out of code without hitting return!")
  in
  let code : zinc_extended = generalize_zinc code in
  let rec loop code env stack =
    match apply_once code env stack with
    | Done -> Interpreter_output.Success (env, stack)
    | Failwith s -> Interpreter_output.Failure s
    | Internal_error s -> failwith s
    | Continue (code, env, stack) -> loop code env stack
  in
  loop code env stack
