let rec my_rec_fun (rounds : int) : unit = 
    if rounds > 0 then
        my_rec_fun (rounds - 1)
    else 
        ()


let run_my_function (f : int -> unit) = 
    f 10

let test = 
    run_my_function my_rec_fun

let test_mutation = 
    match Test.mutation_test my_rec_fun run_my_function with 
    | None -> () 
    | Some (_, mutation) -> let () = Test.log(mutation) in 
                                    failwith "Some mutation also passes the tests!"
