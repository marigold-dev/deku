#import "error_monad.mligo" "M"

type t = M.t

let foo (x : int t) (y : int t) =
  M.bind x (fun (x : int) ->
  M.bind y (fun (y : int) ->
  if (y = 0) then
    (M.fail "Division by zero" : int t)
  else
    (M.ret (x / y))
  ))

let bar (x : int) = M.run (foo (M.ret 5) (M.ret x))
