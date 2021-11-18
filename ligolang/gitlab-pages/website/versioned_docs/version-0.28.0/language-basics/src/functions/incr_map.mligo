let increment (b : int) : int = (fun (a : int) -> a + 1) b

let incr_map (l : int list) : int list =
  List.map (fun (i : int) -> i + 1) l
