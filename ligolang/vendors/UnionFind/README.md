# Some implementations in OCaml of the Union/Find algorithm

All modules implementing Union/Find can be coerced by the same
signature `Partition.S`.

Note the function `alias` which is equivalent to `equiv`, but not
symmetric: `alias x y` means that `x` is an alias of `y`, which
translates in the present context as `x` not being the representative
of the equivalence class containing the equivalence between `x` and
`y`. The function `alias` is useful when managing aliases during the
static analyses of programming languages, so the representatives of
the classes are always the original object.

The module `PartitionMain` tests each with the same equivalence
relations.

## `Partition0.ml`

This is a naive, persistent implementation of Union/Find featuring an
asymptotic worst case cost of O(n^2).

## `Partition1.ml`

This is a persistent implementation of Union/Find with height-balanced
forests and without path compression, featuring an asymptotic worst
case cost of O(n*log(n)).

## `Partition2.ml`

This is an alternate version of `Partition1.ml`, using a different
data type.

## `Partition3.ml`

This is a destructive implementation of Union/Find with
height-balanced forests but without path compression, featuring an
asymptotic worst case of O(n*log(n)). In practice, though, this
implementation should be faster than the previous ones, due to a
smaller multiplicative constant term.
