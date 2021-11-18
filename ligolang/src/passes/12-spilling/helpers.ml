module AST = Ast_typed
module Append_tree = Tree.Append


module LMap = AST.Types.LMap

let list_of_lmap    m = List.rev @@ LMap.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_lmap m = List.rev @@ LMap.fold (fun k v prev -> (k, v) :: prev) m []
let list_of_map m = List.rev @@ Map.String.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_map m = List.rev @@ Map.String.fold (fun k v prev -> (k, v) :: prev) m []
let lmap_of_kv_list lst =
  let open LMap in
  List.fold_left ~f:(fun prev (k, v) -> add k v prev) ~init:empty lst
let map_of_kv_list lst =
  let open Map.String in
  List.fold_left ~f:(fun prev (k, v) -> add k v prev) ~init:empty lst