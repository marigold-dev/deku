#import "glob_a.mligo" "Glob_a"
#import "glob_b.mligo" "Glob_b"

type a = string
let a_v = fun (i:a) -> (i , Glob_a.ga_v 1 , Glob_b.gb_v 1n)