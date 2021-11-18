module B = {
    type titi = int
}

module A = {
    type titi = B.titi
    module C = {
        let toto: titi = 42
    }
    let add = ((a,b) : (titi,titi)) : titi => a + b
}

module D = A

let toto : D.titi = 
    module E = A.C;
    E.toto 

let add = ((a,b) : (A.titi, D.titi)) : A.titi => A.add (a,b)