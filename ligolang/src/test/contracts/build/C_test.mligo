#import "A.mligo" "A"
#import "B.mligo" "B"

let tata = A.toto + B.titi

let foo = B.f ((),3)

let test = assert (tata = 44)