type heap_elt is int * string

function heap_elt_lt (const x : heap_elt;
                      const y : heap_elt) : bool is x.0 < y.0

#include "heap.ligo"
