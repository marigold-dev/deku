// Test a PascaLIGO function which uses other functions as subroutines

function inc (const i : int) : int is i+1

function double_inc (const i : int) : int is inc (i+1)

function foo (const i : int) : int is inc (i) + double_inc (i)
