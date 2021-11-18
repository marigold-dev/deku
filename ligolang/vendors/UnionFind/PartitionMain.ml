module Int =
  struct
    type t = int
    let compare (i: int) (j: int) = Pervasives.compare i j
    let to_string = string_of_int
  end

module Test (Part: Partition.S with type item = Int.t) =
  struct
    open Part

    let () =
      empty
      |> equiv 4 3
      |> equiv 3 8
      |> equiv 6 5
      |> equiv 9 4
      |> equiv 2 1
      |> equiv 8 9
      |> equiv 5 0
      |> equiv 7 2
      |> equiv 6 1
      |> equiv 1 0
      |> equiv 6 7
      |> equiv 8 0
      |> equiv 7 7
      |> equiv 10 10
      |> print
      |> Buffer.contents
      |> print_string
  end


module Test0 = Test (Partition0.Make(Int))
let () = print_newline ()

module Test1 = Test (Partition1.Make(Int))
let () = print_newline ()

module Test2 = Test (Partition2.Make(Int))
let () = print_newline ()

module Test3 = Test (Partition3.Make(Int))
