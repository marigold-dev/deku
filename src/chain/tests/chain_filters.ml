open Deku_crypto
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
  let print = function
    | Universal -> "Universal"
    | Existential _ -> "anything else"
  in
  let a = eou ffrom mfrom in
  let b = eou fto_ mto_ in
  let c = eou fkind mkind in
  Format.eprintf "%s is equal to %s is %b\n%!" (print ffrom)
    (Key_hash.to_b58 mfrom) a;
  Format.eprintf "%s is equal to %s is %b\n%!" (print fto_)
    (Key_hash.to_b58 mto_) b;
  Format.eprintf "%s is equal to %s is %b\n%!" (print fkind)
    (Chain_messages.pp_message_kind mkind)
    c;
  Format.eprintf "catch is %b\n%!"
    (eou ffrom mfrom && eou fto_ mto_ && eou fkind mkind);
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

let _filter_messages filter messages_to_receive =
  Map.map
    (fun messages ->
      List.filter
        (fun message ->
          if catch filter message then (
            Format.eprintf "Universal filter suceeded!\n%!";
            false)
          else (
            Format.eprintf "Universal filter failed!\n%!";
            true))
        messages)
    messages_to_receive

let _filter_messages filters messages_to_receive =
  let messages =
    List.fold_left
      (fun messages_to_receive filter ->
        _filter_messages filter messages_to_receive)
      messages_to_receive filters
  in
  Format.eprintf "message count after %d\n%!"
    (Chain_messages.message_count messages);
  messages
