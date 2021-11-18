type storage is record [fn : option (int -> int); value : int]

type parameter is CallFunction | SetFunction of int -> int

function call (const fn : option (int -> int); const value : int) is
  case fn of [
    Some (f) -> f (value)
  | None -> (failwith ("Lambda is not set") : int)
  ]

function main (const p : parameter; const s : storage) is
block {
  const newStorage
  = case p of [
      SetFunction (fn) -> s with record [fn = Some (fn)]
    | CallFunction -> s with record [value = call (s.fn, s.value)]
    ]
} with ((list [] : list (operation)), newStorage)
