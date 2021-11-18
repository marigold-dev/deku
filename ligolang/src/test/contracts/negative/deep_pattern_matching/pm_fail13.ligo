type storage is int

type action is Decrement of nat | Increment of nat

function main (const p: action ; const s: storage) is
  block {
    const stor =
      case p of [
        Increment (n) -> s + 1
      | Decrement -> s - 1
    ]
  } with ((list [] : list (operation)), stor)
