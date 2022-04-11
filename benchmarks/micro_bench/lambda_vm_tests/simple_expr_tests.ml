let script_incr = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))]

(* decrement script *)

let script_decr = [%lambda_vm.script fun x -> (x - 1L, (0L, 0L))]

(* pair *)

let script_pair =
  [%lambda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))]

(* if *)

let script_if =
  [%lambda_vm.script
    fun param ->
      ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]

(* lambda *)

let script_lambda =
  let incr_lamdba = [%lambda_vm fun x -> x + 1L] in
  let decr_lambda = [%lambda_vm fun x -> x - 1L] in
  [%lambda_vm.script
    fun param ->
      ( (fun inc -> if inc then [%e incr_lamdba] else [%e decr_lambda])
          (fst param) (snd param),
        (0L, 0L) )]

(* lambda application *)

let script_lambda_app = [%lambda_vm.script fun y -> (fun x -> (x, (0L, 0L))) y]
