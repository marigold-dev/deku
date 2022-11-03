open Craft_protocol

module Chunk_map = Map.Make (struct
  type t = coord

  let compare = compare_coord
end)

type cache_key = int
type blocks = block_kind Chunk_map.t

module World_map = Map.Make (struct
  type t = chunk

  let compare = compare_chunk
end)

module Int_map = Map.Make (Int)

type state = {
  world : (cache_key * blocks) World_map.t;
  players : player_state Int_map.t;
}

let empty = { world = World_map.empty; players = Int_map.empty }

type effect = Broadcast of Server_commands.t | Respond of Server_commands.t

let chunked x = x / Constants.chunk_size

let send_block chunk x y z w =
  [ Broadcast (Block { chunk; x; y; z; w }); Broadcast (Redraw chunk) ]

let cross_product a b = List.map (fun x -> List.map (fun y -> (x, y)) b) a

let handle_client_connect ~client_id state =
  let player_id = client_id in
  let position = { x = 0.; y = 0.; z = 0.; rx = 0.; ry = 0. } in
  let nick = Format.sprintf "guest%d" client_id in
  let effects =
    [
      Respond (You { player_id = client_id; player_position = position });
      Respond
        (Time
           { elapsed = Unix.gettimeofday (); day_length = Constants.day_length });
      Respond (Talk "Welcome to Craft!");
      Respond (Talk "Type /help for a list of commands.");
      Broadcast (Position { player_id; player_position = position });
      Broadcast (Nick { player_id; name = nick });
    ]
    @ Int_map.fold
        (fun player_id player_state acc ->
          Respond
            (Position { player_id; player_position = player_state.position })
          :: Respond (Nick { player_id; name = player_state.nick })
          :: acc)
        state.players []
  in
  let players = Int_map.add client_id { nick; position } state.players in
  ({ state with players }, effects)

let handle_client_command ~client_id state =
  let open Client_commands in
  function
  | Version | Authentication ->
      (* unhandled for now *)
      (state, [])
  | Chunk { p; q; key } -> (
      let chunk = { p; q } in
      match World_map.find_opt { p; q } state.world with
      | Some (cache_key, blocks) ->
          if cache_key > key then
            (* TODO: respond with lights and signs *)
            let effects =
              Chunk_map.fold
                (fun (x, y, z) w acc ->
                  Respond (Block { chunk; x; y; z; w }) :: acc)
                blocks []
            in
            let effects =
              effects
              @ [ Respond (Key { chunk; cache_key }); Respond (Redraw chunk) ]
            in
            (state, effects)
          else (state, [])
      | None -> (state, []))
  | Block { x; y; z; w } -> (
      print_endline "In block!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11";
      let p, q = (chunked x, chunked z) in
      let chunk = { p; q } in
      let cache_key, blocks =
        Option.value ~default:(0, Chunk_map.empty)
          (World_map.find_opt chunk state.world)
      in
      let previous = Chunk_map.find_opt (x, y, z) blocks in
      match previous with
      | _ when y <= 0 || y > 255 ->
          print_endline "1";
          (state, [ Respond (Talk "Invalid block coordinates") ])
      | _ when not @@ List.mem w Constants.allowed_items ->
          print_endline "2";
          (state, [ Respond (Talk "That item is not allowed") ])
      (* | None when w = 0 ->
          print_endline "3";
          (state, [ Respond (Talk "That space is already empty") ]) *)
      | Some previous when List.mem previous Constants.indestructible_items ->
          print_endline "4";
          (state, [ Respond (Talk "Cannot destroy that type of block") ])
      | Some _ | None ->
          print_endline "Comitting block!!!!!!!!!!!!!!!!!!!!!!!";
          let blocks = Chunk_map.add (x, y, z) w blocks in
          let world = World_map.add chunk (cache_key + 1, blocks) state.world in
          (* I don't know the purpose of this boundary fix thing
             original python code:
             for dx in range(-1, 2):
                   for dz in range(-1, 2):
                       if dx == 0 and dz == 0:
                           continue
                       if dx and chunked(x + dx) == p:
                           continue
                       if dz and chunked(z + dz) == q:
                           continue
                       np, nq = p + dx, q + dz
                       self.execute(query, dict(p=np, q=nq, x=x, y=y, z=z, w=-w))
                       self.send_block(client, np, nq, x, y, z, -w)
          *)
          (* let boundary = [ -1; 0; 1; 2 ] in
             let boundary_fix_effects =
               List.fold_left
                 (fun acc pair -> match pair with _ ->
                   (* TODO: finish this  *)
                   assert false)
                 []
                 (cross_product boundary boundary)
             in *)
          let boundary_fix_effects = [] in
          let effects = send_block chunk x y z w @ boundary_fix_effects in
          ({ state with world }, effects @ boundary_fix_effects))
  | Position player_position ->
      ( state,
        [ Broadcast (Position { player_id = client_id; player_position }) ] )
  | Talk message -> (state, [ Broadcast (Talk message) ])
  | Light _ | Sign _ -> (state, [])
