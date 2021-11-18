open QCheck

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val get_one : ?n:int -> 'a t -> ((Location.t * Ast_typed.expression) list * 'a)
  val get_list : ?n:int -> 'a t -> ((Location.t * Ast_typed.expression) list * 'a) list
  val oneof : 'a t list -> 'a t
  val mutate_int : int -> int t
  val mutate_nat : int -> int t
  val mutate_string : string -> string t
  val location : Location.t -> Ast_typed.expression -> unit t
  val frequency : (int * 'a t) list -> 'a t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module Rnd : Monad = struct
  type 'a t = ((Location.t * Ast_typed.expression) list * 'a) Gen.t

  let return x : 'a t = Gen.return ([], x)
  let (let*) (x : 'a t) (f : 'a -> 'b t) : 'b t =
    Gen.(x >>= fun (l, x) ->
      f x >>= fun (l', r) ->
      return (l @ l', r))
  let get_one ?n (x : 'a t) =
    let rand = match n with
      | None -> Random.State.make_self_init()
      | Some seed ->
         let curr = Random.get_state () in
         Random.init seed;
         let rand = Random.get_state () in
         Random.set_state curr;
         rand in
    Gen.generate1 ~rand x
  let get_list ?(n = 100) (l : 'a t) =
    Gen.generate ~n l
  let oneof (l : 'a t list) : 'a t = Gen.oneof l
  let frequency (l : (int * 'a t) list) : 'a t = Gen.frequency l
  let mutate_int z = Gen.oneof [Gen.return ([], z)] (* ; Gen.small_int *)
  let mutate_nat n = Gen.oneof [Gen.return ([], n)] (* n; Gen.big_nat *)
  let mutate_string s = Gen.oneof [Gen.return ([], s)]  (* [Gen.return s; Gen.map String.escaped (Gen.small_string ~gen:Gen.printable)] *)
  let location (loc : Location.t) (e : Ast_typed.expression) : unit t =
    Gen.return ([(loc, e)], ())
end

module Lst : Monad = struct
  type 'a t = ((Location.t * Ast_typed.expression) list * 'a) list

  let return x = [([], x)]
  let (let*) x f = List.concat (List.map ~f:(fun (l, x) -> List.concat (List.map ~f:(fun (l', x) -> [(l @ l', x)]) (f x))) x)
  let get_one ?(n = 0) l = Option.value (List.nth l n) ~default:(List.last_exn l)
  let get_list ?n l =
    let n = Option.value n ~default:(List.length l) in
    List.take l n
  let oneof l = List.concat l
  let mutate_int n = [([],n)]
  let mutate_nat n = [([],n)]
  let mutate_string s = [([],s)]
  let location (loc : Location.t) (e : Ast_typed.expression) : unit t =
    [([(loc, e)], ())]
  let frequency l = List.concat @@ List.map ~f:snd l
end

module Monad_context (M : Monad) = struct
  include M

  let bind_location (x:_ Location.wrap) =
    let* wrap_content = x.wrap_content in
    return { x with wrap_content }

  let rec bind_list = function
      [] -> return []
    | hd::tl -> let* hd = hd in
                let* tl = bind_list tl in
                return @@ hd :: tl

  let bind_map_list f lst = bind_list (List.map ~f lst)

  let bind_map_option f = function
      None -> return None
    | Some s -> let* x = f s in
                return (Some x)

  let bind_map_location f x = bind_location (Location.map f x)

  let bind_and (a, b) =
    let* a = a in
    let* b = b in
    return (a, b)

  let bind_and3 (a, b, c) =
    let* a = a in
    let* b = b in
    let* c = c in
    return (a, b, c)

  let bind_pair = bind_and

  let bind_map_pair f (a, b) =
    bind_pair (f a, f b)

  let bind_fold_list f init lst =
    let aux x y =
      let* x = x in f x y
    in List.fold_left ~f:aux ~init:(return init) lst

  let bind_fold_ne_list f init lst =
    let aux x y =
      let* x = x in f x y
    in Simple_utils.List.Ne.fold_left aux (return init) lst

  let bind_ne_list (hd, tl) =
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ (hd, tl)

  let bind_map_ne_list : _ -> 'a Simple_utils.List.Ne.t -> 'b Simple_utils.List.Ne.t t =
    fun f lst -> bind_ne_list (Simple_utils.List.Ne.map f lst)

  let map f x =
    let* x = x in
    return (f x)

  let perhaps_oneof x l =
    let l = List.map ~f:(fun x -> (1, map (fun x -> (x, true)) x)) l in
    let l = (2 * List.length l + 10,return (x, false)) :: l in
    frequency l

  let ( let+ ) = fun x f -> map f x

  let ( and+ ) = fun x y ->
    let* x = x in
    let* y = y in
    return @@ (x, y)

  let rec traverse_list f = function
    | [] -> return []
    | (x :: xs) ->
       let+ x = f x
       and+ xs = traverse_list f xs in
       x :: xs

end
