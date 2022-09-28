module N = N
module Uri = Uri_ext
include Let_syntax
module Parallel = Parallel

module Yojson = struct
  include Yojson

  module Safe = struct
    include Safe

    let t_of_yojson t = t
    let yojson_of_t t = t
  end
end

module Trace = struct
  let dump msg = Format.printf "%s %f\n%!" msg (Unix.gettimeofday ())
  let outer_timer = Atomic.make (Unix.gettimeofday ())
  let inner_timer = Atomic.make (Unix.gettimeofday ())

  let _start () =
    let now = Unix.gettimeofday () in
    let elapsed = now -. Atomic.get outer_timer in
    Format.printf "Total Elapsed time: %3f\n%!" elapsed;
    Atomic.set outer_timer now;
    Atomic.set inner_timer now

  let _trace msg =
    let now = Unix.gettimeofday () in
    let elapsed = now -. Atomic.get inner_timer in
    Format.printf "%s: %3f\n%!" msg elapsed;
    Atomic.set inner_timer now
end
