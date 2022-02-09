
module Domain_types = struct
  module TezosAddress = Tezos.Address
  module Key = Crypto.Key
  module Address = Address
  module Hash = Crypto.BLAKE2B
  module Chain_id = Chain_id
  module Ticket = Ticket_table.Handle
  module Key_hash = Crypto.Key_hash
  module Contract = Contract
end

include Zinc_interpreter.Make(Domain_types)
