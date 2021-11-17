type balances = map(address, tez);

let balances_under = ( (b, threshold) : (balances, tez) ) : balances => 
  let f = ( (acc,(k,v)) : (balances, (address, tez)) ) =>  if (v < threshold) { Map.remove (k,acc) } else {acc} ;
  Map.fold (f,b,b)