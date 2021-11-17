(* DIGITS AND CODES *)

(* Encodings of non-empty binary trees with PAIR and UNPAIR. *)

type digit = P | A | I
type code  = digit list

(* Injection and projection for digits *)

let char_to_digit = function
  'P' -> P
| 'A' -> A
| 'I' -> I
|   _ -> assert false (* The lexer must ensure this case never happens. *)

let digit_to_char = function
  P -> 'P'
| A -> 'A'
| I -> 'I'

(* Injection and projection for codes *)

let lift string =
  let length = String.length string in
  let rec push acc index =
    if index = -1 then acc
    else let digit = char_to_digit string.[index]
         in push (digit::acc) (index-1)
  in push [] (length-1)

let drop code =
  let length = List.length code in
  let bytes  = Bytes.make length ' ' in
  let rec fill index = function
      [] -> bytes
  | d::l -> Bytes.set bytes index (digit_to_char d);
           fill (index+1) l
  in fill 0 code |> Bytes.to_string

(* NON-EMPTY BINARY TREES *)

(* Equivalent design with a GADT and polymorphic variants:

type _ t =
  Left  : [> `Left] t
| Right : [> `Right] t
| Pair  : [< `Left | `Pair] t * [< `Right | `Pair] t -> [> `Pair] t
*)

type status = Fake | Real
type left   = [`Left  of status | `Pair of left * right]
and  right  = [`Right of status | `Pair of left * right]
and  tree   = [`Pair of left * right]

type t = tree

(* Decoding trees *)

type index = int

type child = [`Left | `Right]

(* The type [mode] denotes the current state of the decoding: either a
   valid code (yielding a correct tree), or an invalid one (which
   has/had to be completed with fake leaves to show the recognised
   structure and what needs fixing). In the latter case, the index is
   the offset in the code of the first occurrence of an incorrect or
   unexpected pair constructor, and the type [child] records whether
   the error occurred when decoding a left subtree or a right
   subtree. This information will be useful when printing the error
   message with a hint about what to do about it. *)

type mode = Valid | Invalid of child * index

(* The type [attribute] is named so to evoke the terminology of
   attribute grammars. Here, a value of the type [attribute] will be
   threaded along the control flow of the parsing process (that is,
   the decoding), thus acting both as a synthesised and an inherited
   attribute. The field [mode] was described above. The field [suffix]
   is the remainder of the code to be decoded, and [index] is the
   offset with respect to the original code of the first constructor
   in [suffix]. In other words, it is the length of the longest valid
   prefix so far. *)

type attribute = {
  index  : int;
  suffix : code;
  mode   : mode
}

(* The call [invalidate c a] updates the attribute [a] to the status
   "invalid", and records the child [c] and the current index. *)

let invalidate child attr =
  match attr.mode with
    Valid   -> {attr with mode = Invalid (child, attr.index)}
  | _       -> attr

(* The call [shift a] assumes that the suffix code in the attribute
   [a] is not empty. If not empty, it increments the index and take
   the tail of the suffix. This implements the discarding of the
   current token in recursive-descent parsing to read the next token
   (here, a pair constructor). *)

let shift attr =
  match attr.suffix with
    _::suffix -> {attr with index = attr.index + 1; suffix}
  |        [] -> assert false

(* The functions [cast_left] and [cast_right] upcast the first
   component of their parameter, which is a tree, respectively to a left
   subtree and a right subtree. *)

let cast_left  (tree, attr) = (tree :> left),  attr
let cast_right (tree, attr) = (tree :> right), attr

(* Due to the precise type definitions [tree], [left] and [right], we
   need three decoding functions, one for each: [decode],
   [decode_left] and [decode_right], respectively. All take an
   attribute and return an updated version of it, together with the
   tree they have decoded. This means that trees are purely synthetic
   attributes, that is, they are built bottom-up with respect to the
   control flow.

     This can be best seen in [decode], which checks for a `P'
   constructor before decoding a left subtree, then a right subtree,
   and finally building a binary tree with those. Note the call to
   [shift] to discard `P' after it has been matched. Also, the pattern
   matching can fail if anything other than the `P' constructor is
   currently to be read. If the encoding comes from the lexer (as it
   should if this module is not used independently), this assumption
   is satisfied by the regular expression there that matches the
   prefix of the lexing buffer and that must start with a `P'.

     The decodings of left and right subtrees are isomorphic. Both
   check for either a `P' constructor or the one corresponding to
   their orientation (`A' if decoding a left subtree, `I'
   otherwise). Note the casts ([cast_left] and [cast_right]) which are
   needed for type inference. In case of failure to match the expected
   constructors, a fake leaf is made and the mode is set to "invalid"
   (see function [invalidate] above). Note that functions [shift] and
   [invalidate] do not commute.

     Finally, the exported decoding function is actually shadowing the
   function [decode] described above, as the clients of this module
   should not be made aware of any attributes, as those are merely a
   tool to build the trees. Therefore, the exported [decode] function
   only takes a code as parameter.
*)

let rec decode attr : tree * attribute =
  match attr.suffix with
    P::_ -> let left,  attr = decode_left (shift attr) in
           let right, attr = decide_right attr
           in `Pair (left, right), attr
  |    _ -> assert false

and decode_left attr : left * attribute =
  match attr.suffix with
    P::_ -> decode attr |> cast_left
  | A::_ -> `Left Real, shift attr
  | I::_ -> `Left Fake, shift (invalidate `Left attr)
  |   [] -> `Left Fake, invalidate `Left attr

and decide_right attr : right * attribute =
  match attr.suffix with
    P::_ -> decode attr |> cast_right
  | A::_ -> `Right Fake, shift (invalidate `Right attr)
  | I::_ -> `Right Real, shift attr
  |   [] -> `Right Fake, invalidate `Right attr

type decode_err =
  Valid_prefix of index * tree
| Invalid_tree of index * tree * child

let decode code =
  match decode {index=0; suffix=code; mode=Valid} with
    tree, {mode=Valid; suffix=[]; _} ->
      Stdlib.Ok tree
  | tree, {mode=Valid; index; _} ->
      Error (Valid_prefix (index, tree))
  | tree, {mode = Invalid (child, index); _} ->
      Error (Invalid_tree (index, tree, child))

(* Encoding of trees

   The encoding of a binary tree is the preorder traversal of its
   nodes.
*)

let rec preorder_l code = function
       `Left _ -> A :: code
| `Pair _ as p -> preorder code p

and preorder_r code = function
      `Right _ -> I :: code
| `Pair _ as p -> preorder code p

and preorder code : t -> code = function
  `Pair (left, right) -> P :: preorder_l (preorder_r code right) left

let encode tree = preorder [] tree

(* Conversions and I/Os *)

let draw channel tree =
  let rec draw_l ~pad:(pd,pc) = function
      `Left Real -> Printf.fprintf channel "%sA\n" pd
  |   `Left Fake -> Printf.fprintf channel "%sX\n" pd
  | `Pair _ as p -> draw ~pad:(pd,pc) p

  and draw_r ~pad:(pd,pc) = function
     `Right Real -> Printf.fprintf channel "%sI\n" pd
  |  `Right Fake -> Printf.fprintf channel "%sX\n" pd
  | `Pair _ as p -> draw ~pad:(pd,pc) p

  and draw ~pad:(pd,pc) = function
    `Pair (left, right) -> Printf.fprintf channel "%sP\n" pd;
                          draw_r ~pad:(pc ^ "|-- ", pc ^ "|   ") right;
                          draw_l ~pad:(pc ^ "`-- ", pc ^ "    ") left

  in draw ~pad:("","") tree

let to_string tree =
  let rec to_string_l ~pad:(pd,pc) = function
      `Left Real -> Printf.sprintf "%sA\n" pd
  |   `Left Fake -> Printf.sprintf "%sX\n" pd
  | `Pair _ as p -> to_string ~pad:(pd,pc) p

  and to_string_r ~pad:(pd,pc) = function
     `Right Real -> Printf.sprintf "%sI\n" pd
  |  `Right Fake -> Printf.sprintf "%sX\n" pd
  | `Pair _ as p -> to_string ~pad:(pd,pc) p

  and to_string ~pad:(pd,pc) = function
    `Pair (left, right) ->
      Printf.sprintf "%sP\n%s%s"
        pd
        (to_string_r ~pad:(pc ^ "|-- ", pc ^ "|   ") right)
        (to_string_l ~pad:(pc ^ "`-- ", pc ^ "    ") left)
  in to_string ~pad:("","") tree

(* PATHS IN BINARY TREES *)

type path = child list

let char_to_selector = function
  'A' -> `Left
| 'D' -> `Right
|   _ -> assert false

let lift_path string =
  let length = String.length string in
  let rec push acc index =
    if index = 0 then acc
    else let selector = char_to_selector string.[index]
         in push (selector::acc) (index-1)
  in push [] (length-2)

let drop_path forest =
  let length = List.length forest + 2 in
  let bytes  = Bytes.make length ' ' in
  let     () = Bytes.set bytes 0 'C';
               Bytes.set bytes (length-1) 'R' in

  let rec fill index = function
              [] -> bytes
  | `Left ::path -> Bytes.set bytes index 'A'; fill (index+1) path
  | `Right::path -> Bytes.set bytes index 'D'; fill (index+1) path

  in fill 1 forest |> Bytes.to_string
