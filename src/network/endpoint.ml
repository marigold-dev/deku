open Deku_concepts
open Deku_protocol
open Deku_consensus

type _ endpoint =
  | Blocks : Block.t endpoint
  | Signatures : Verified_signature.t endpoint
  | Operations : Operation.t endpoint
  | Bootstrap : Bootstrap_signal.t endpoint

type 'a t = 'a endpoint
type ex = Ex : _ endpoint -> ex

let blocks = Blocks
let signatures = Signatures
let operations = Operations
let bootstrap = Bootstrap

module Blocks_route = struct
  let path () = Routes.(s "consensus" / s "blocks" /? nil)
  let parser () = Routes.(path () @--> Ex Blocks)
end

module Signatures_route = struct
  let path () = Routes.(s "consensus" / s "signatures" /? nil)
  let parser () = Routes.(path () @--> Ex Signatures)
end

module Operations_route = struct
  let path () = Routes.(s "operations" /? nil)
  let parser () = Routes.(path () @--> Ex Operations)
end

module Bootstrap_route = struct
  let path () = Routes.(s "consensus" / s "bootstrap" /? nil)
  let parser () = Routes.(path () @--> Ex Bootstrap)
end

let routes =
  Routes.one_of
    [
      Blocks_route.parser ();
      Signatures_route.parser ();
      Operations_route.parser ();
      Bootstrap_route.parser ();
    ]

let of_string path = Routes.match' routes ~target:path

let to_string (type a) (endpoint : a endpoint) =
  match endpoint with
  | Blocks -> Routes.sprintf (Blocks_route.path ())
  | Signatures -> Routes.sprintf (Signatures_route.path ())
  | Operations -> Routes.sprintf (Operations_route.path ())
  | Bootstrap -> Routes.sprintf (Bootstrap_route.path ())
