
module Executor = struct
  module Key = struct 
    include Crypto.Key
    let hash_key t = to_string t |> Crypto.BLAKE2B.hash
  end
  module Address = struct
    include Tezos.Address
  end
  module Hash = struct
    include Crypto.BLAKE2B
  end
  module Chain_id = Chain_id
  module Contract = Contract
end

include Zinc_interpreter.Make(Executor)
