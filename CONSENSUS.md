# Consensus

When to update the burn_receipts? Always? Together with state_root? What are the problems

```haskell
sequence2 :: forall a. forall b. a -> b -> b
sequence1 :: forall a. a -> forall b. b -> b

sequence3 = (sequence1 :: forall a. forall b. a -> b -> b)
```
