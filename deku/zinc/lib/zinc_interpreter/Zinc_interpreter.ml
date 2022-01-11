open Zinc_utils
open Zinc_types
open Zinc_interpreter_intf

module Make (D : Domain_types) = struct
  module Types = Zinc_types.Make (D)

  module type Executor =
    Executor
      with type key := Types.Zinc.Key.t
      with type key_hash := Types.Zinc.Key_hash.t
       and type address := Types.Zinc.Address.t
       and type chain_id := Types.Zinc.Chain_id.t
       and type ticket := Types.Zinc.Ticket.t
       and type hash := Types.Zinc.Hash.t
       and type contract := Types.Zinc.Contract.t

  module Interpreter = struct
    open Types

    external env_to_stack : Env_item.t -> Stack_item.t = "%identity"

    external stack_to_env_ext : Stack_item.t -> Env_item.t = "%identity"

    let stack_to_env = function
      | Stack_item.Marker _ ->
          failwith
            "type error, cant convert a stack_item marker into an env_item"
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

    let stack_of pod =
      let open Zinc in
      [Stack_item.Z (Plain_old_data pod)]

    let has_err l =
      let rec aux (acc_l, acc_r) l' =
        match l' with
        | [] -> (acc_l, acc_r)
        | Ok h :: t -> aux (h :: acc_l, acc_r) t
        | Error _ :: t -> aux (acc_l, true) t
      in
      aux ([], false) l

    let initial_state ?initial_stack:(stack = []) a : Interpreter_input.t =
      (a, [], stack)

    let[@warning "-4"] eval (module E : Executor) (code, env, stack) =
      let rec stack_of_pack_result r =
        let open Zinc in
        let open E in
        match r with
        | Pack.Int z -> Ok (stack_of (Num z))
        | Pack.String s -> Ok (stack_of (String s))
        | Pack.Bytes b -> Ok (stack_of (Bytes b))
        | Pack.Key k -> Ok (stack_of (Key k))
        | Pack.Key_hash kh -> Ok (stack_of (Key_hash kh))
        | Pack.Address a -> Ok (stack_of (Address a))
        | Pack.Error s -> Error (Steps.Internal_error s)
        | Pack.List l -> (
            match has_err (List.map stack_of_pack_result l) with
            | (_, true) ->
                Error
                  (Steps.Internal_error
                     "Found invalid entry while unpacking list")
            | (results, _) -> Ok (List.concat results @ stack_of Nil))
      in
      let pack_pod d =
        let open E in
        let open Zinc in
        match d with
        | Num n -> Ok (Pack.int n)
        | Bytes b -> Ok (Pack.bytes b)
        | String s -> Ok (Pack.string s)
        | Key k -> Ok (Pack.key k)
        | Key_hash kh -> Ok (Pack.key_hash kh)
        | Address a -> Ok (Pack.address a)
        | _ -> Error (Steps.Internal_error "Tried to pack unpackable type")
      in

      let apply_once (code : Zinc.t) env stack =
        let open Zinc in
        match (code, env, stack) with
        | (Operation Pack :: c, env, si :: stack) ->
            let open E in
            let open Base.With_return in
            with_return (fun r ->
                let packed_data =
                  match si with
                  | Stack_item.Z (Plain_old_data d) -> (
                      match pack_pod d with
                      | Ok res -> res
                      | Error e -> r.return e)
                  | Stack_item.List l ->
                      l
                      |> List.filter_map (function
                             | Stack_item.Z (Plain_old_data d) -> Some d
                             | _ -> None)
                      |> List.map pack_pod |> has_err
                      |> fun (result_list, result_flag) ->
                      if result_flag then Pack.list result_list
                      else
                        r.return
                          (Steps.Internal_error
                             "Found invalid entry while packing list")
                  | _ ->
                      r.return (Steps.Internal_error "Non-packable stack item")
                in
                let si =
                  Stack_item.Z
                    (Plain_old_data (Bytes (Pack.to_bytes packed_data)))
                in
                Steps.Continue (c, env, si :: stack))
        | (Operation Unpack :: c, env, Stack_item.Z (Plain_old_data d) :: stack)
          ->
            let open E in
            let open Base.With_return in
            with_return (fun r ->
                let packed_data =
                  match d with
                  | Bytes b -> b
                  | _ ->
                      r.return
                        (Steps.Internal_error "Tried to unpack non-byte data")
                in
                let unpacked_data =
                  packed_data |> Pack.of_bytes |> stack_of_pack_result
                in
                match unpacked_data with
                | Ok stack_items -> Steps.Continue (c, env, stack_items @ stack)
                | Error internal_error -> internal_error)
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
        | (Core Grab :: _, _, []) -> Steps.Failwith "nothing to grab!"
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
                    (0, Stack_item.(NonliteralValue (Contract contract)))
              | None -> Stack_item.Variant (1, Utils.unit_record_stack)
            in
            Steps.Continue (c, env, contract :: s)
        (* TODO: fix, MakeTransaction is totally wrong,   *)
        | ( Domain_specific_operation MakeTransaction :: c,
            env,
            r
            :: Stack_item.Z (Plain_old_data (Mutez _amount))
               :: Stack_item.(Z (Plain_old_data (Address address)) :: s ))
          when Stack_item.equal r Utils.unit_record_stack ->
            Steps.Continue
              ( c,
                env,
                Stack_item.(NonliteralValue
                  (Chain_operation (Transaction (Utils.unit_record_stack, address))))
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
                 "%s unimplemented!"
                 (Zinc.instruction_to_string x))
        | _ ->
            Steps.Internal_error
              (Format.asprintf
                 "somehow ran out of code without hitting return!")
      in
      let rec loop code env stack =
        match apply_once code env stack with
        | Steps.Done -> Interpreter_output.Success (env, stack)
        | Steps.Failwith s -> Interpreter_output.Failure s
        | Steps.Internal_error s -> Interpreter_output.Failure s
        | Steps.Continue (code, env, stack) -> loop code env stack
      in
      loop code env stack
  end
end

module Dummy = Make (Zinc_types.Dummy_domain)
