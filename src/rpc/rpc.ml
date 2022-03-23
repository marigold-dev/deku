open Graphql_lwt.Schema

(* TODO: wrappers to force doc*)

let int64_typ =
  scalar "int64" ~coerce:(fun block_height -> `Int (Int64.to_int block_height))

let height_field ~resolve =
  field "height" ~typ:(non_null int64_typ) ~args:[] ~resolve:(fun _info () ->
      resolve ())

let make_schema ~resolve_height : unit schema =
  schema [height_field ~resolve:resolve_height]

(*
   let x = enum_value "lol" ~value:()

   let scalar_string ~doc name parse =
     let open Arg in
     let coerce payload =
       let%ok string =
         match payload with
         | `String string -> Ok string
         | _ -> Error (Format.sprintf "%s must be a string" name) in
       match parse string with
       | Some value -> Ok value
       | None -> Error (Format.sprintf "%S is not a valid %s" string name) in
     non_null (scalar ~doc name ~coerce)

   (* TODO: docs *)
   let blake2b =
     (* TODO: doc *)
     scalar_string ~doc:"DOCV" "BLAKE2B" BLAKE2B.of_string

   let key = scalar_string ~doc:"DOCV" "Key" Key.of_string

   let signature = scalar_string ~doc:"DOCV" "Signature" Signature.of_string

   type block_signature = {
     hash : BLAKE2B.t;
     key : Key.t;
     signature : Signature.t;
   }

   let blake2b =
     scalar "blake2b" ~coerce:(fun hash -> `String (BLAKE2B.to_string hash))

   let x =
     obj "block_signature"
       ~fields:
         [
           field "hash" ~typ:(non_null blake2b) ~args:[]
             ~resolve:(fun _info block_signature -> block_signature.hash);
         ]
   let fields =
     Arg.
       [
         arg "hash" ~typ:blake2b; arg "key" ~typ:key; arg "signature" ~typ:signature;
       ]
   let block_signature =
     let open Arg in
     obj "block_signature"
       ~fields:
         [
           arg "hash" ~typ:blake2b;
           arg "key" ~typ:key;
           arg "signature" ~typ:signature;
         ]
       ~coerce:(fun hash key signature -> { hash; key; signature })
   let x = Arg.arg ?doc:None
   type block_signature = {
     hash : BLAKE2B.t;
     key : Key.t;
     signature : Signature.t;
   }
   [@@deriving graphql { doc = "DOCV" }]

   type sign_block = block_signature -> unit [@@deriving graphql { doc = "DOCV" }]

   (* TODO: better return for sign_block *)
   let unit = non_null int

   let handle_sign_block ~hash ~key ~signature = 1

   let sign_block =
     field "sign_block" ~doc:"DOCV" ~typ:unit
       ~args:
         Arg.
           [
             arg "hash" ~typ:blake2b;
             arg "key" ~typ:key;
             arg "signature" ~typ:signature;
           ]
       ~resolve:(fun _info () hash key signature ->
         handle_sign_block ~hash ~key ~signature)
   let x = schema [sign_block]
   module Make (P : sig
     val handle_sign_block :
       hash:BLAKE2B.t -> key:Key.t -> signature:Signature.t -> int
   end) =
   struct end
   let x = io_field
   (* let block_signature = obj "block_and_signature" ~fields:(fun _info -> [
        field "hash" ~typ:(int)
      ]) *)
*)
