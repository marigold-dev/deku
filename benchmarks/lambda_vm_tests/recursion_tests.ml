let rec fac = function
  | 0L -> 1L
  | n -> Int64.(mul n (fac (sub n 1L)))

let factorial_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n -> if n then n * f f (n - 1L) else 1L),
        (0L, 0L) )]

(* fibonacci *)
let rec fib = function
  | 0L
  | 1L ->
    1L
  | n -> Int64.add (fib (Int64.sub n 1L)) (fib (Int64.sub n 2L))

let fibonacci_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if (0L - n) * (1L - n) then f f (n - 2L) + f f (n - 1L) else 1L),
        (0L, 0L) )]
