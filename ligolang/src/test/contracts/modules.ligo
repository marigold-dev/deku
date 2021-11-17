module B is {
    type titi is int
}

module A is {
    type titi is int
    module C is {
        const toto : titi = 42
    }
    function add (const a : int; const b : int) : int is a + b
}

module D is A

const toto : D.titi = block {
    module E is A.C;
} with E.toto

function add (const a : A.titi; const b : D.titi) : A.titi is A.add (a,b)