open Protocol
open Helpers
open Crypto

let pool_ref = ref None

let initial_height_ref = ref 0L

(** query that returns unit **)
let unit query param (module C : Caqti_lwt.CONNECTION) = C.exec query param

(** query that returns exactly one element **)
let one query param (module C : Caqti_lwt.CONNECTION) = C.find query param

(** query that returns exactly an option **)
let option query param (module C : Caqti_lwt.CONNECTION) =
  C.find_opt query param

let exec_query exec query param =
  let exec = exec query param in
  match !pool_ref with
  | None -> await (Error "The database is not initialized")
  | Some pool ->
    let%await result = Caqti_lwt.Pool.use exec pool in
    result |> Result.map_error Caqti_error.show |> await

let add (block : Block.t) =
  let hash = BLAKE2B.to_string block.hash in
  let level = block.block_height in
  let block_json = Block.to_yojson block |> Yojson.Safe.to_string in
  let query =
    let open Caqti_request.Infix in
    let open Caqti_type.Std in
    (tup3 string int64 string ->. unit)
    @@ "insert into blocks (hash, height, block) values (?, ?, ?)" in
  let%await result = exec_query unit query (hash, level, block_json) in
  result |> Result.map (fun _ -> block) |> await

let create_table_query =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  (unit ->. unit)
  @@ {| create table if not exists blocks (
    hash varchar not null primary key,
    height int not null,
    block text
  )|}

let init ~uri initial_height =
  let pool = Caqti_lwt.connect_pool uri in
  let pool =
    match pool with
    | Ok pool -> pool
    | Error err -> failwith (Caqti_error.show err) in
  pool_ref := Some pool;

  (* Creates a table |block_hash:string|block_height:int64|block:json|*)
  let%await result = exec_query unit create_table_query () in
  (match result with
  | Ok _ -> print_endline "table initialized" (*TODO: replace with log*)
  | Error err -> failwith err);

  (* Adds the genesis block *)
  let%await _ = add Block.genesis in

  (* Set the initial height *)
  initial_height_ref := initial_height;
  await ()

let level () =
  let query =
    let open Caqti_request.Infix in
    let open Caqti_type.Std in
    (unit ->! int64) @@ "SELECT MAX(height) FROM blocks" in
  let%await result = exec_query one query () in
  match result with
  | Ok level -> await (Int64.max level !initial_height_ref)
  | Error err -> failwith err

let find_block_by_level level =
  let query =
    let open Caqti_request.Infix in
    let open Caqti_type.Std in
    (int64 ->? string) @@ "select block from blocks where height = ?" in
  let%await result = exec_query option query level in
  let result =
    match result with
    | Ok block -> block
    | Error err -> failwith err in
  result
  |> Option.map Yojson.Safe.from_string
  |> Option.map Block.of_yojson
  |> Option.map Result.to_option
  |> Option.join
  |> await