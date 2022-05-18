exception Invalid_value

type t = {
  sender : unit -> Ir.value;
  source : unit -> Ir.value;
  gas : Gas.t;
}

let memo f =
  let memoized = ref None in
  fun () ->
    !memoized
    |> Option.fold
         ~some:Fun.id
         ~none:
           (let res = f () in
            memoized := Some res ;
            res)

let make : sender:string -> source:string -> Gas.t -> t =
 fun ~sender ~source gas ->
  let source =
    memo (fun () ->
        source
        |> Ast.value_of_string gas
        |> Compiler.compile_value gas
        |> Result.fold ~ok:Fun.id ~error:(fun _ -> raise Invalid_value)) in
  let sender =
    memo (fun () ->
        sender
        |> Ast.value_of_string gas
        |> Compiler.compile_value gas
        |> Result.fold ~ok:Fun.id ~error:(fun _ -> raise Invalid_value)) in
  {sender; source; gas}

let[@inline always] sender t = t.sender

let[@inline always] source t = t.source

let[@inline always] gas t = t.gas
