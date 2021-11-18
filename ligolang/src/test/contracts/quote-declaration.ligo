function foo (const input : int) : int is input + 42

function main (const i : int) : int is i + foo (i)
