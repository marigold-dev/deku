open Deku_stdlib

module Id : sig
  type id
  type t = id

  val initial : t
  val next : t -> t
  val encoding : t Data_encoding.t

  module Map : Map.S with type key = t
end = struct
  type id = int
  and t = id

  let initial = 0
  let next t = t + 1
  let encoding = Data_encoding.int31

  module Map = Map.Make (struct
    type t = int [@@deriving ord]

    let encoding = Data_encoding.int31
  end)
end

module Message = struct
  type message = { id : Id.t; content : Data_encoding.Json.t }
  and t = message

  let encoding =
    let open Data_encoding in
    conv
      (fun { id; content } -> (id, content))
      (fun (id, content) -> { id; content })
      (obj2
         (req "id" (dynamic_size Id.encoding))
         (req "content" Data_encoding.Json.encoding))
end

module Pending : sig
  exception Unknown_id of Id.t

  type pending
  type t = pending

  val empty : pending
  val listen : (Data_encoding.Json.t -> unit) -> pending -> pending * Id.t
  val request : (Data_encoding.Json.t -> unit) -> pending -> pending * Id.t
  val incoming : Id.t -> Data_encoding.Json.t -> pending -> pending
end = struct
  exception Unknown_id of Id.t

  type kind = Listen | Request

  type pending =
    | Pending of {
        next_id : Id.t;
        resolvers : (kind * (Data_encoding.Json.t -> unit)) Id.Map.t;
      }

  type t = pending

  let empty = Pending { next_id = Id.initial; resolvers = Id.Map.empty }

  let append kind resolver pending =
    let (Pending { next_id; resolvers }) = pending in
    let id = next_id in
    let next_id = Id.next id in
    let resolvers = Id.Map.add id (kind, resolver) resolvers in
    (Pending { next_id; resolvers }, id)

  let listen resolver pending = append Listen resolver pending
  let request resolver pending = append Request resolver pending

  let incoming id json pending =
    let (Pending { next_id; resolvers }) = pending in
    match Id.Map.find_opt id resolvers with
    | Some (Request, resolver) ->
        resolver json;
        let resolvers = Id.Map.remove id resolvers in
        Pending { next_id; resolvers }
    | Some (Listen, resolver) ->
        resolver json;
        Pending { next_id; resolvers }
    | None -> raise (Unknown_id id)
end

module Protocol : sig
  val handler :
    (read:(unit -> Message.t) -> write:(Message.t -> unit) -> 'a) ->
    stdin:Eio.Flow.sink ->
    stdout:Eio.Flow.source ->
    'a
end = struct
  (* TODO: magic number: 256mb *)
  let max_size = 256 * 1024 * 1024

  let message_line buf =
    let line = Eio.Buf_read.line buf in
    let json = Data_encoding.Json.from_string line in
    let json = match json with Ok json -> json | _ -> failwith "" in
    Data_encoding.Json.destruct Message.encoding json

  let write_message_line buf message =
    (* TODO: check max size *)
    let json = Data_encoding.Json.construct Message.encoding message in
    let line = Data_encoding.Json.to_string json in
    Eio.Buf_write.string buf line;
    Eio.Buf_write.char buf '\n'

  let handler k ~stdin ~stdout =
    Eio.Buf_write.with_flow stdin @@ fun write_buf ->
    let read_buf = Eio.Buf_read.of_flow ~max_size stdout in
    let read () = message_line read_buf in
    let write message = write_message_line write_buf message in
    k ~read ~write
end

type js_process = { mutable pending : Pending.t; write : Message.t -> unit }
type t = js_process

let make ~write = { pending = Pending.empty; write }

let spawn ~file k =
  let prog = "node" in
  let args = [| prog; file |] in
  Logs.info (fun m -> m "js_process.spawn: prog: %s, file: %s" prog file);
  IO.spawn ~prog ~args @@ Protocol.handler
  @@ fun ~read ~write ->
  let process = make ~write in
  let on_message message =
    let Message.{ id; content } = message in
    process.pending <- Pending.incoming id content process.pending
  in
  let rec loop () =
    let message = read () in
    on_message message;
    loop ()
  in
  let never, () = Eio.Fiber.pair (fun () -> loop ()) (fun () -> k process) in
  never

let listen process content ~on_message =
  let pending, id = Pending.listen on_message process.pending in
  process.pending <- pending;
  process.write Message.{ id; content }

let request process content =
  let promise, resolver = Eio.Promise.create () in
  let () =
    let resolver json = Eio.Promise.resolve resolver json in
    let pending, id = Pending.request resolver process.pending in
    process.pending <- pending;
    process.write Message.{ id; content }
  in
  Eio.Promise.await promise
