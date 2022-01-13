open Zinc_utils
open Zinc_types
open Zinc_interpreter_intf

module Make (D : Domain_types) = struct
  module Ir = Zinc_instructions.Instructions.Make (D)
  module Types = Ir.Zt

  module type Executor =
    Executor
      with type key := D.Key.t
      with type key_hash := D.Key_hash.t
       and type address := D.Address.t
       and type contract := D.Contract.t
       and type chain_id := D.Chain_id.t
       and type hash := D.Hash.t

  module Interpreter = struct
    open Types

    external env_to_stack : Env_item.t -> Stack_item.t = "%identity"

    external stack_to_env_ext : Stack_item.t -> Env_item.t = "%identity"

    let stack_to_env = function
      | Stack_item.Marker _ ->
          failwith "type error, cant convert a stack_item into an env_item"
      | Stack_item.(
          Clos _ | Record _ | Variant _ | List _ | Z _ | NonliteralValue _) as x
        ->
          stack_to_env_ext x

    module Steps = struct
      type t =
        | Done
        | Internal_error of string
        | Failwith of string
        | Continue of Zinc.t * Env.t * Stack.t
    end

    let initial_state ?initial_stack:(stack = []) a : Interpreter_input.t =
      (a, [], stack)

    let[@warning "-4"] eval (module E : Executor) (code, env, stack) =
      let apply_once (code : Zinc.t) env stack =
        (* let _ =
             print_endline
               (Format.asprintf
                  "interpreting:\ncode:  %s\nenv:   %s\nstack: %s"
                  (Zinc.to_string code)
                  (Env.to_string env)
                  (Stack.to_string stack))
           in *)
        let open Zinc in
        match (code, env, stack) with
        | ( Operation Or :: c,
            env,
            (Stack_item.Z (Plain_old_data (Bool x)) as x')
            :: (Stack_item.Z (Plain_old_data (Bool _)) as y') :: stack ) ->
            let return = if x then x' else y' in
            Steps.Continue (c, env, return :: stack)
        | ( Operation And :: c,
            env,
            (Stack_item.Z (Plain_old_data (Bool x)) as x')
            :: (Stack_item.Z (Plain_old_data (Bool _)) as y') :: stack ) ->
            let return = if x then y' else x' in
            Steps.Continue (c, env, return :: stack)
        | ( Operation Not :: c,
            env,
            Stack_item.Z (Plain_old_data (Bool x)) :: stack ) ->
            let return = Stack_item.Z (Plain_old_data (Bool (not x))) in
            Steps.Continue (c, env, return :: stack)
        | (Plain_old_data Nil :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.List [] :: s)
        | (Operation Cons :: c, env, item :: Stack_item.List x :: s) ->
            Steps.Continue (c, env, Stack_item.List (item :: x) :: s)
        | (Core Grab :: c, env, Stack_item.Marker (c', e') :: s) ->
            Steps.Continue
              (c', e', Stack_item.Clos {Clos.code = Core Grab :: c; env} :: s)
        | (Core Grab :: c, env, v :: s) ->
            Steps.Continue (c, stack_to_env v :: env, s)
        | (Core Grab :: _, _, []) -> failwith "nothing to grab!"
        | ( Core Return :: _,
            _,
            Stack_item.Z v :: Stack_item.Marker (c', e') :: s ) ->
            Steps.Continue (c', e', Stack_item.Z v :: s)
        | (Core Return :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s)
          ->
            Steps.Continue (c', e', s)
        | (Core (PushRetAddr c') :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.Marker (c', env) :: s)
        | (Core Apply :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s)
          ->
            Steps.Continue (c', e', s)
        (* Below here is just modern SECD *)
        | (Core (Access n) :: c, env, s) -> (
            let nth = Base.List.nth env n in
            match nth with
            | Some nth -> Steps.Continue (c, env, (nth |> env_to_stack) :: s)
            | None ->
                Steps.Internal_error "Tried to access env item out of bounds")
        | (Core (Closure c') :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.Clos {Clos.code = c'; env} :: s)
        | (Core EndLet :: c, _ :: env, s) -> Steps.Continue (c, env, s)
        (* zinc extensions *)
        (* operations that jsut drop something on the stack haha *)
        | ( (Plain_old_data
               ( Num _ | Address _ | Key _ | Hash _ | Bool _ | String _
               | Mutez _ | Bytes _ ) as v)
            :: c,
            env,
            s ) ->
            Steps.Continue (c, env, Stack_item.Z v :: s)
        (* ADTs *)
        | (Adt (MakeRecord r) :: c, env, s) ->
            let list_split_at ~n lst =
              let rec go n acc = function
                | [] ->
                    if Int.equal n 0 then (acc, [])
                    else
                      raise (Invalid_argument "not enough entries on the list")
                | x when Int.equal n 0 -> (acc, x)
                | x :: xs -> go (n - 1) (x :: acc) xs
              in
              go n [] lst
            in
            let (record, stack) = list_split_at ~n:r s in
            let record_contents = LMap.of_list (List.rev record) in
            Steps.Continue (c, env, Stack_item.Record record_contents :: stack)
        | (Adt (RecordAccess accessor) :: c, env, Stack_item.Record r :: s) ->
            let res =
              let res = LMap.find r accessor in
              Steps.Continue (c, env, res :: s)
            in
            res
        | ( Adt (MatchVariant vs) :: c,
            env,
            Stack_item.Variant (label, item) :: s ) ->
            let match_code = LMap.find vs label in
            Steps.Continue (List.concat [match_code; c], env, item :: s)
        | ( Adt (MatchVariant vs) :: c,
            env,
            Stack_item.Z (Plain_old_data (Bool b)) :: s ) ->
            let label = if b then 1 else 0 in
            let item = Utils.unit_record_stack in
            let match_code = LMap.find vs label in
            Steps.Continue (List.concat [match_code; c], env, item :: s)
        | (Adt (MakeVariant label) :: c, env, value :: s) ->
            Steps.Continue (c, env, Stack_item.Variant (label, value) :: s)
        (* Math *)
        | ( Operation Add :: c,
            env,
            Stack_item.Z (Plain_old_data (Num a))
            :: Stack_item.Z (Plain_old_data (Num b)) :: s ) ->
            Steps.Continue
              (c, env, Stack_item.Z (Plain_old_data (Num (Z.add a b))) :: s)
        | ( Operation Add :: c,
            env,
            Stack_item.Z (Plain_old_data (Mutez a))
            :: Stack_item.Z (Plain_old_data (Mutez b)) :: s ) ->
            Steps.Continue
              (c, env, Stack_item.Z (Plain_old_data (Mutez (Z.add a b))) :: s)
        (* Booleans *)
        | (Operation Eq :: c, env, a :: b :: s) ->
            (* This is not constant time, which is bad *)
            Steps.Continue
              ( c,
                env,
                Stack_item.Z (Plain_old_data (Bool (Stack_item.equal a b))) :: s
              )
        (* Crypto *)
        | ( Operation HashKey :: c,
            env,
            Stack_item.Z (Plain_old_data (Key key)) :: s ) ->
            let h = E.key_hash key in
            Steps.Continue
              (c, env, Stack_item.Z (Plain_old_data (Key_hash h)) :: s)
        (* Tezos specific *)
        | (Domain_specific_operation ChainID :: c, env, s) ->
            Steps.Continue
              ( c,
                env,
                Stack_item.Z
                  (* TODO: fix this usage of Digestif.BLAKE2B.hmac_string - should use an effect system or smth.
                     Also probably shouldn't use key like this. *)
                  (Plain_old_data (Chain_id E.chain_id)) :: s )
        | ( Domain_specific_operation Contract_opt :: c,
            env,
            Stack_item.Z (Plain_old_data (Address address)) :: s ) ->
            (* todo: abstract this into a function *)
            let contract =
              match E.get_contract_opt address with
              | Some contract ->
                  Stack_item.Variant
                    (0, Stack_item.NonliteralValue (Contract contract))
              | None -> Stack_item.Variant (1, Utils.unit_record_stack)
            in
            Steps.Continue (c, env, contract :: s)
        | ( Domain_specific_operation MakeTransaction :: c,
            env,
            r
            :: Stack_item.Z (Plain_old_data (Mutez amount))
               :: Stack_item.NonliteralValue (Contract contract) :: s )
          when Stack_item.equal r Utils.unit_record_stack ->
            Steps.Continue
              ( c,
                env,
                Stack_item.NonliteralValue
                  (Chain_operation (Transaction (amount, contract)))
                :: s )
        (* should be unreachable except when program is done *)
        | ([Core Return], _, _) -> Steps.Done
        | ( Control_flow Failwith :: _,
            _,
            Stack_item.Z (Plain_old_data (String s)) :: _ ) ->
            Steps.Failwith s
        (* should not be reachable *)
        | (x :: _, _, _) ->
            Steps.Internal_error
              (Format.asprintf
                 "%s unimplemented!\n stack: %s "
                 (Zinc.instruction_to_string x)
                 (Stack.to_string stack))
        | _ ->
            Steps.Internal_error
              (Format.asprintf
                 "somehow ran out of code without hitting return!")
      in
      let rec loop code env stack =
        match apply_once code env stack with
        | Steps.Done -> Interpreter_output.Success (env, stack)
        | Steps.Failwith s -> Interpreter_output.Failure s
        | Steps.Internal_error s -> failwith s
        | Steps.Continue (code, env, stack) -> loop code env stack
      in
      loop code env stack

    let unit_record = Ir.Record [||]

    module G = Gas_counter_intf.Dummy_Gas (Ir)

    let[@warning "-4"] eval' (module E : Executor) ~debug (code, env, stack) =
      let counter = G.make () in
      let rec loop code env stack gas =
        let (_ : unit) =
          if debug then
            print_endline
              (Base.Printf.sprintf
                 "interpreting:\ncode:  %s\nenv:   %s\nstack: %s"
                 ([%derive.show: Ir.t list] code)
                 ([%derive.show: Ir.t list] env)
                 ([%derive.show: Ir.t list] stack))
          else ()
        in
        let open Ir in
        match (code, stack) with
        | ([Return], _) -> (env, stack)
        | (Or :: c, (Bool x as x') :: (Bool _ as y') :: stack) ->
            let return = if x then x' else y' in
            loop c env (return :: stack) (G.simple gas)
        | (And :: c, (Bool x as x') :: (Bool _ as y') :: stack) ->
            let return = if x then y' else x' in
            loop c env (return :: stack) (G.simple gas)
        | (Not :: c, Bool x :: stack) ->
            let return = Bool (not x) in
            loop c env (return :: stack) (G.simple gas)
        | (Nil :: c, s) -> loop c env (List [] :: s) gas
        | (Cons :: c, item :: List x :: s) ->
            loop c env (List (item :: x) :: s) (G.simple gas)
        | (Grab :: _, Marker {code = c'; env = e'} :: s) ->
            loop c' e' (Clos {code; env} :: s) (G.simple gas)
        | (Grab :: c, v :: s) -> loop c (v :: env) s (G.simple gas)
        | (Grab :: _, []) -> failwith "nothing to grab!"
        | (Return :: _, v :: Marker {code = c'; env = e'} :: s) ->
            loop c' e' (v :: s) (G.simple gas)
        | (Return :: _, Clos {code = c'; env = e'} :: s) ->
            loop c' e' s (G.simple gas)
        | (PushRetAddr c' :: c, s) ->
            loop c env (Marker {code = c'; env} :: s) (G.simple gas)
        | (Apply :: _, Clos {code = c'; env = e'} :: s) ->
            loop c' e' s (G.simple gas)
        (* Below here is just modern SECD *)
        | (Access n :: c, s) ->
            loop c env (Base.List.nth_exn env n :: s) (G.simple_n gas n)
        | (Closure c' :: c, s) ->
            loop c env (Clos {code = c'; env} :: s) (G.simple gas)
        | (EndLet :: c, s) -> loop c (List.tl env) s gas
        (* zinc extensions *)
        (* operations that jsut drop something on the stack haha *)
        | ( (( Num _ | Address _ | Key _ | Hash _ | Bool _ | String _ | Mutez _
             | Bytes _ ) as v)
            :: c,
            s ) ->
            loop c env (v :: s) (G.sized gas ~item:v)
        (* ADTs *)
        | (MakeRecord r :: c, s) ->
            let (record, stack) = Base.List.split_n s r in
            let record_contents = LMap.of_list record in
            loop c env (Record record_contents :: stack) (G.simple_n gas r)
        | (RecordAccess accessor :: c, Record r :: s) ->
            let res = LMap.find r accessor in
            loop c env (res :: s) (G.simple gas)
        | (MatchVariant vs :: c, Variant {tag = label; value = item} :: s) ->
            let match_code = LMap.find vs label in
            loop (List.concat [match_code; c]) env (item :: s) (G.simple gas)
        | (MatchVariant vs :: c, Bool b :: s) ->
            let label = if b then 1 else 0 in
            let match_code = LMap.find vs label in
            loop
              (List.concat [match_code; c])
              env
              (unit_record :: s)
              (G.simple gas)
        | (MakeVariant label :: c, value :: s) ->
            loop
              c
              env
              (Variant {tag = label; value} :: s)
              (G.sized gas ~item:value)
        (* Math *)
        | (Add :: c, Num a :: Num b :: s) ->
            loop c env (Num (Z.add a b) :: s) (G.simple gas)
        | (Add :: c, Mutez a :: Mutez b :: s) ->
            loop c env (Mutez (Z.add a b) :: s) (G.simple gas)
        (* Booleans *)
        | (Eq :: c, a :: b :: s) ->
            (* This is not constant time, which is bad *)
            loop c env (Bool (Ir.equal a b) :: s) (G.simple gas)
        (* Crypto *)
        | (HashKey :: c, Key key :: s) ->
            let h = E.key_hash key in
            loop c env (Key_hash h :: s) (G.simple gas)
        (* Tezos specific *)
        | (ChainID :: c, s) ->
            loop
              c
              env
              (* TODO: fix this usage of Digestif.BLAKE2B.hmac_string - should use an effect system or smth.
                 Also probably shouldn't use key like this. *)
              (Chain_id E.chain_id :: s)
              (G.simple gas)
        | (Contract_opt :: c, Address address :: s) ->
            (* todo: abstract this into a function *)
            let contract =
              match E.get_contract_opt address with
              | Some contract -> Variant {tag = 0; value = Contract contract}
              | None -> Variant {tag = 1; value = unit_record}
            in
            loop c env (contract :: s) (G.simple gas)
        | ( MakeTransaction :: c,
            Record [||] :: Mutez amount :: Contract contract :: s ) ->
            loop c env (Transaction (amount, contract) :: s) (G.simple gas)
        (* should be unreachable except when program is done *)
        | (Failwith :: _, String s :: _) -> failwith s (* should be a failure *)
        (* should not be reachable *)
        | (x :: _, _) ->
            failwith (Format.asprintf "%s unimplemented!" (Ir.show x))
        | _ ->
            failwith
              (Format.asprintf
                 "somehow ran out of code without hitting return!")
      in
      loop code env stack counter
  end
end

module Dummy_domain = struct
  open Bin_prot.Std

  module Hash = struct
    type t = string [@@deriving eq, yojson, bin_io]

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x
  end

  module Address = struct
    type t = string [@@deriving eq, yojson, bin_io]

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x
  end

  module Contract = struct
    type t = string * string option [@@deriving show, eq, yojson, bin_io]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option
  end

  module Key = struct
    type t = string [@@deriving eq, yojson, bin_io]

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x
  end

  module Key_hash = struct
    type t = string [@@deriving eq, yojson, bin_io]

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x
  end

  module Chain_id = struct
    type t = string [@@deriving eq, yojson, bin_io]

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x
  end
end

module Dummy = Make (Dummy_domain)
