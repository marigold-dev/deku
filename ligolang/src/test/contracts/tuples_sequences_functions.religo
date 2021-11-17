let a = 1;
let b = 1n;
let c = 2mutez;
let d = 1n + 2n;
let e = 1mutez + 3mutez;
let f = (a, c);
let g = (a + 1, c);
let h = ("a" ++ "2", d);
let i = (a: int, b: int) => a + b;
let j = (a: int, b: int) => a - b;
/* not supported by typer yet: let k = () => b; */
/* not supported by typer yet: let l = () => i(2,3); */
let m = {
    let z = 3;
    z;
};
let n = (a: int): int => a + 1;
let o = (a: int): int => a + 1;
let n = (a: int, _b: int): int => a + 1;
let o = (a: int, _b: int): int => a + 1;
let p = {{
    3;
}};
let q = {
    f: 3,
    g: 6,
    h: {
        i: "bla",
        j: 1 + 2,
        k: {
            l: 1,
            z: 2
        },
    },
};

/*
Not supported yet by parser:

let r = {
  a: 1
};
*/

let s = {
    let a = 2;
    {
      z: a,
      a
    };
};

let t = (((((((2)))))));
let u = if (true) { 1; } else { 2; };
