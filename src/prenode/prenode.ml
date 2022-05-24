module Validators_Parameter = struct
  module Threshold = struct
    let get _height = 3 (* TODO: FIXME: not 3 *)
  end
end

module Validators_Prenode = Validators_prenode.Raw (Validators_Parameter)

module Protocol_Parameter = struct
  let toto _msg = "toto"
end

module Protocol = Protocol_prenode.Raw (Protocol_Parameter)

module Message = Message
