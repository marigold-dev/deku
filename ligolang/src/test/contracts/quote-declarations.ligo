function foo (const input : int) : int is input + 23

function bar (const input : int) : int is input + 51

function main (const i : int) : int is foo (i) + bar (i)
