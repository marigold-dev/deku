type storage is unit

type return is list (operation) * storage

function cb (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) = get_entrypoint ("%cb", a)
  } with (list [Tezos.transaction (unit, 0tez, c)], s)


function cbo (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) =
      case (get_entrypoint_opt ("%cbo", a) : option (contract (unit))) of
        Some (c) -> c
      | None -> (failwith ("cbo: Entrypoint not found.") : contract (unit))
      end
  } with (list [Tezos.transaction (unit, 0tez, c)], s)
