#import "nelist.mligo" "Ne"

let foo = Ne.to_list (Ne.map (fun (x : int) -> x * 2) (Ne.of_list [1;2;3]))
let bar = Ne.fold_left (fun ((x, y) : int * int) -> x + y) 0 (Ne.map (fun (x : int) -> x * 2) (Ne.of_list [1;2;3]))

