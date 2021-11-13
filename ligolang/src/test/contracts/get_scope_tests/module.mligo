module A = struct
    let toto = 1
end

let a = A.toto

module B = A

let b = B.toto

let titi = 
    module C = struct
        let a = 4
    end in
    module D = C in
    D.a