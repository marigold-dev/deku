
module Domain_types = struct
  module Key = Crypto.Key
  module Address = Tezos.Address
  module Hash = Crypto.BLAKE2B
  module Chain_id = Chain_id
  module Key_hash = Crypto.Key_hash
  module Contract = Contract
end

include Zinc_interpreter.Make(Domain_types)
