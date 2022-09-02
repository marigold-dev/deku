open Deku_concepts
open Deku_protocol
open Deku_consensus

type 'a post = Post of 'a
type get = Get

type _ endpoint =
  | Blocks : Block.t post endpoint
  | Signatures : Verified_signature.t post endpoint
  | Operations : Operation.t post endpoint
  | Bootstrap : Bootstrap_signal.t post endpoint
  | Get_block_by_level : Level.t -> get endpoint
  | Get_genesis : get endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

let blocks = Blocks
let signatures = Signatures
let operations = Operations
let bootstrap = Bootstrap

module Blocks_route = struct
  let path () = Routes.(s "consensus" / s "blocks" /? nil)
  let parser () = Routes.(path () @--> Ok (Ex Blocks))
end

module Signatures_route = struct
  let path () = Routes.(s "consensus" / s "signatures" /? nil)
  let parser () = Routes.(path () @--> Ok (Ex Signatures))
end

module Operations_route = struct
  let path () = Routes.(s "operations" /? nil)
  let parser () = Routes.(path () @--> Ok (Ex Operations))
end

module Bootstrap_route = struct
  let path () = Routes.(s "consensus" / s "bootstrap" /? nil)
  let parser () = Routes.(path () @--> Ok (Ex Bootstrap))
end

module Get_block_by_level_route = struct
  let path () = Routes.(s "chain" / s "blocks" / str /? nil)

  let parser () =
    Routes.(
      path () @--> fun level ->
      level |> Level.of_b58
      |> Option.to_result ~none:(Internal_error.invalid_level level)
      |> Result.map (fun level -> Ex (Get_block_by_level level)))
end

module Get_genesis = struct
  let path () = Routes.(s "chain" / s "blocks" / s "genesis" /? nil)
  let parser () = Routes.(path () @--> Ok (Ex Get_genesis))
end

let routes =
  Routes.one_of
    [
      Blocks_route.parser ();
      Signatures_route.parser ();
      Operations_route.parser ();
      Bootstrap_route.parser ();
      Get_block_by_level_route.parser ();
      Get_genesis.parser ();
    ]

let parse ~path ~meth =
  let route = Routes.match' routes ~target:path in
  match route with
  | None -> Error (Internal_error.endpoint_not_found path)
  | Some (Error error) -> Error error
  | Some (Ok route) -> (
      let (Ex endpoint) = route in
      match (endpoint, meth) with
      | Blocks, `POST
      | Signatures, `POST
      | Operations, `POST
      | Bootstrap, `POST
      | Get_block_by_level _, `GET
      | Get_genesis, `GET ->
          Ok route
      | Blocks, meth
      | Signatures, meth
      | Operations, meth
      | Bootstrap, meth
      | Get_block_by_level _, meth
      | Get_genesis, meth ->
          Error (Internal_error.method_not_allowed meth path))

let to_string (type a) (endpoint : a endpoint) =
  match endpoint with
  | Blocks -> Routes.sprintf (Blocks_route.path ())
  | Signatures -> Routes.sprintf (Signatures_route.path ())
  | Operations -> Routes.sprintf (Operations_route.path ())
  | Bootstrap -> Routes.sprintf (Bootstrap_route.path ())
  | Get_block_by_level level ->
      Routes.sprintf (Get_block_by_level_route.path ()) (Level.to_b58 level)
  | Get_genesis -> Routes.sprintf (Get_genesis.path ())
