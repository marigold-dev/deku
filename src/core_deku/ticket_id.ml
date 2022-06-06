open Bin_prot.Std

type t = Tezos.Ticket_id.t = {
  ticketer : Tezos.Address.t;
  data : bytes;
}
[@@deriving eq, ord, yojson, bin_io]
