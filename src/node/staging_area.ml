type t = (Crypto.BLAKE2B.t * int64 * int, Protocol.Signature.t list) Hashtbl.t

let create () = Hashtbl.create 0

let add t hash height round signature =
  begin
    match Hashtbl.find_opt t (hash, height, round) with
    | Some xs ->
      let xs = signature :: xs in
      Hashtbl.add t (hash, height, round) xs
    | None -> Hashtbl.add t (hash, height, round) [signature]
  end;
  t

let get t hash height round =
  Hashtbl.find_opt t (hash, height, round) |> Option.value ~default:[]

let contains t hash height round signature =
  get t hash height round |> List.mem signature

let nb_of_signatures t hash height round =
  get t hash height round |> List.length
