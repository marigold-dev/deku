open Deku_ledger

type t =
  | Entry : {
      encoded_module : string;
      storage : Value.t;
      originated_by : Address.t;
      entrypoints : Entrypoints.t;
      constants : (int * Value.t) array;
      module_ : Wasm.Ast.module_';
    }
      -> t
[@@deriving show, ord]

val make :
  entrypoints:Entrypoints.t ->
  encoded_module:string ->
  storage:Value.t ->
  constants:(int * Value.t) array ->
  source:Address.t ->
  module_:Wasm.Ast.module_' ->
  unit ->
  t

val update : t -> storage:Value.t -> t
val encoding : t Data_encoding.t
