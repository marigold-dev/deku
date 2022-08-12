open Deku_concepts

val apply :
  sender:Address.t ->
  operation_hash:Operation_hash.t ->
  level:Level.t ->
  nonce:Nonce.t ->
  apply_operation_content:
    (operation_hash:Operation_hash.t ->
    source:Address.t ->
    level:Level.t ->
    nonce:Nonce.t ->
    Contract_storage.t ->
    Ledger.t ->
    Operation.operation_content ->
    (Ledger.t * Contract_storage.t) * Receipt.t) ->
  Ledger.t ->
  Contract_storage.t ->
  Contract_operation.t ->
  (Ledger.t * Contract_storage.t) * Receipt.t
