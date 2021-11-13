let works = (0 : timestamp)

module Does_work = struct
  let does_work = (0 : timestamp)
end

let main (_ : unit * timestamp) : operation list * timestamp =
  (([] : operation list), works)
