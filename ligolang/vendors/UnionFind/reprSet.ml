include PolySet

let alias ?debug ~demoted_repr ~new_repr s =
  if PolySet.mem demoted_repr s then
    if PolySet.mem new_repr s then
      PolySet.remove ?debug demoted_repr s
    else
      PolySet.add ?debug new_repr (PolySet.remove ?debug demoted_repr s)
  else
    s
