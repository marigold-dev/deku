module Timestep = struct
  module Raw = struct
    type t = int
    module Millisecond = struct
      let of_int : int -> t = fun i -> i
      let to_int : t -> int = fun i -> i
    end
  end
  include (
    Raw :
      sig
        type t
        module Millisecond : sig
          val of_int : int -> t
          val to_int : t -> int
        end
      end)
end

module Timestamp = struct
  module Raw = struct
    type t = float
    (** Number of seconds since 1970*)

    let add : t -> Timestep.t -> t =
     fun now duration ->
      let f = 0.001 *. (float_of_int @@ Timestep.Millisecond.to_int duration) in
      now +. f

    let more_recent : t -> t -> bool = fun v1 v2 -> v1 >= v2
  end
  include (
    Raw :
      sig
        type t
        val add : t -> Timestep.t -> t
        val more_recent : t -> t -> bool
      end)
end
