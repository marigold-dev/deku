// Test functions with several parameters in PascaLIGO

function ab (const a : int; const b : int) : int is a+b

function abcd (const a : int;
               const b : int;
               const c : int;
               const d : int) : int is a+b+c+d+2

function abcde (const _a : int;
                const _b : int;
                const c : int;
                const _d : int;
                const e : int) : int is c+e+3
