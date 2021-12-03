type taco_supply = { current_stock : nat , max_price : tez } ;

type taco_shop_storage = map (nat, taco_supply) ;
type return = (list(operation), taco_shop_storage)

let buy_taco = ((taco_kind_index, taco_shop_storage) : (nat, taco_shop_storage)) : return => {
  /* Retrieve the taco_kind from the contract's storage or fail */
  let taco_kind : taco_supply =
    switch (Map.find_opt (taco_kind_index, taco_shop_storage)) {
    | Some k => k
    | None => (failwith ("Unknown kind of taco"): taco_supply) } ;
  let current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock ;
  /* We won't sell tacos if the amount is not correct */
  let x : unit = if (Tezos.amount != current_purchase_price) {
    failwith ("Sorry, the taco you are trying to purchase has a different price")
  } ;
  /* Update the storage decreasing the stock by 1n */
  let taco_shop_storage = Map.update (
    taco_kind_index,
    (Some ({...taco_kind, current_stock : abs (taco_kind.current_stock - 1n) })),
    taco_shop_storage );
  (([]: list(operation)), taco_shop_storage)
};