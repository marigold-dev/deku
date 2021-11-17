open Types

let graph (dep_g,filename) =
  let set = SSet.empty in
  let rec pp_node set name parent =
    let node = ["file",  `String name] in
    if SSet.mem name set then ("child", `Assoc node)::parent
    else
      let set = SSet.add name set in
      let node = G.fold_succ (pp_node set) dep_g name node in
      let node = List.rev node in
      ("child", `Assoc node)::parent
  in
  let root = ["root", `String filename] in
  let root =
    G.fold_succ (pp_node set) dep_g filename root
  in `Assoc (List.rev root)
