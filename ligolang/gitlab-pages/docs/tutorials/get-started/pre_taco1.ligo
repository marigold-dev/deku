type taco_supply is record [ current_stock : nat ; max_price : tez ]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

const init_storage : taco_shop_storage = map [
  1n -> record [ current_stock = 50n ; max_price = 50tez ] ;
  2n -> record [ current_stock = 20n ; max_price = 75tez ] ;
]