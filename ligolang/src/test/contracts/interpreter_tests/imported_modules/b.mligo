#import "glob_a.mligo" "Glob_a"
#import "glob_b.mligo" "Glob_b"

type b = unit
let b_v = fun (i:b) -> (i , Glob_a.ga_v 1 , Glob_b.gb_v 1n)