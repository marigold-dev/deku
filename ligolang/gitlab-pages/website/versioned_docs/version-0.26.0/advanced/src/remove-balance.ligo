type balances is map (address, tez)

function balances_under (const b : balances ; const threshold : tez) is
  block {
    const f =
      function (const x : balances * (address * tez)) is
        block {
          const (acc, (k,v)) = x ;
        } with if v < threshold then Map.remove (k, acc) else acc ;
  } with Map.fold (f, b, b)