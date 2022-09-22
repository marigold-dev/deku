open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_gossip
open Chain_messages
open Chain_genesis

type 'a quantifier = Existential of 'a | Universal

type message_filter =
  | Prevent of {
      from : Key_hash.t quantifier;
      to_ : Key_hash.t quantifier;
      kind : message_kind quantifier;
    }

(* does filter catch message *)
let catch filter message =
  let eou a b =
    match (a, b) with Universal, _ -> true | Existential a, b -> a = b
  in
  let (Prevent { from = ffrom; to_ = fto_; kind = fkind; _ }) = filter in
  let (Message { from = mfrom; to_ = mto_; kind = mkind; _ }) = message in
  eou ffrom mfrom && eou fto_ mto_ && eou fkind mkind

let validators_quant = Universal :: List.map (fun v -> Existential v) validators

let kind_quant =
  [
    Existential Response; Existential Request; Existential Broadcast; Universal;
  ]

let _generate_filter () =
  let from = Random.int 5 in
  let to_ = Random.int 5 in
  let kind = Random.int 4 in
  let from = List.nth validators_quant from in
  let to_ = List.nth validators_quant to_ in
  let kind = List.nth kind_quant kind in
  Prevent { from; to_; kind }

let _filter =
  let validator2 = List.nth validators 2 in
  Prevent { from = Universal; to_ = Existential validator2; kind = Universal }

let _universal_filter =
  Prevent { from = Universal; to_ = Universal; kind = Universal }

(* TODO: Generalize to handle many filters *)
let _filter_messages filter messages_to_receive =
  Map.map
    (fun messages ->
      List.filter (fun message -> not (catch filter message)) messages)
    messages_to_receive
