// The type accountBalances denotes maps from addresses to tez

type account_balances is map (address, tez)

const ledger : account_balances =
  map
   [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> 10mutez]
