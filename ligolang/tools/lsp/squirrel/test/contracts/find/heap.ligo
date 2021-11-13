// Implementation of the heap data structure in PascaLIGO
// See: https://en.wikipedia.org/wiki/Heap_%28data_structure%29

type heap is map (nat, heap_elt)

function is_empty (const h : heap) : bool is size (h) = 0n

function get_top (const h : heap) : heap_elt is get_force (1n, h)

function pop_switch (var h : heap) : heap is
  block {
   const result : heap_elt = get_top (h);
   const s : nat = Map.size (h);
   const last : heap_elt =
     case h[s] of
       Some (e) -> e
     | None -> (failwith ("No element.") : heap_elt)
     end;
   remove 1n from map h;
   h[1n] := last
  } with h

function pop_ (var h : heap) : nat is
  block {
    const result : heap_elt = get_top (h);
    const s : nat = Map.size (h);
    var current : heap_elt :=
      case h[s] of
        Some (e) -> e
      | None -> (failwith ("No element.") : heap_elt)
      end;
    const i : nat = 1n;
    const left : nat = 2n * i;
    const right : nat = left + 1n;
    remove 1n from map h;
    h[1n] := current;
    var largest : nat := i;
    const tmp : heap_elt = get_force (s, h);
    if left <= s and heap_elt_lt (tmp, get_force (left,h))
    then largest := left
    else
      if right <= s and heap_elt_lt (tmp, get_force (right,h))
      then largest := right
      else skip
  } with largest

function insert (var h : heap ; const e : heap_elt) : heap is
  block {
    var i : nat := size (h) + 1n;
    h[i] := e;
    var largest : nat := i;
    var parent : nat := 0n;
    while largest =/= i block {
      parent := i/2n;
      largest := i;
      if parent >= 1n then {
        if heap_elt_lt (get_force (parent,h), get_force(i,h)) then {
          largest := parent;
          const tmp : heap_elt = get_force (i,h);
          h[i] := get_force(parent, h);
          h[parent] := tmp
        } else skip
      } else skip
    }
  } with h

function pop (var h : heap) : heap * heap_elt * nat is
  block {
    const result : heap_elt = get_top (h);
    var s : nat := size (h);
    const last : heap_elt = get_force (s,h);
    remove s from map h;
    h[1n] := last;
    s := size (h);
    var i : nat := 0n;
    var largest : nat := 1n;
    var left : nat := 0n;
    var right : nat := 0n;
    var c : nat := 0n;
    while largest =/= i block {
      c := c + 1n;
      i := largest;
      left := 2n * i;
      right := left + 1n;
      if left <= s then {
        if heap_elt_lt (get_force (left,h), get_force(i,h)) then {
          largest := left;
          const tmp : heap_elt = get_force(i,h);
          h[i] := get_force (left, h);
          h[left] := tmp
        } else skip
      }
      else
        if right <= s then {
          if heap_elt_lt (get_force (right, h), get_force (i,h)) then {
            largest := right;
            const tmp : heap_elt = get_force (i,h);
            h[i] := get_force (right, h);
            h[left] := tmp
          } else skip
        } else skip
    };
    while False block { skip; }
  } with (h, result, c)

const empty : heap = map []
