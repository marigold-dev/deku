open Tezos_micheline
open Micheline

type 'l michelson = ('l, string) node
type 'l t = 'l michelson

let prim ?(annot=[]) ?(children=[]) p : unit michelson =
  Prim ((), p, children, annot)

let annotate annot = function
  | Prim (l, p, c, []) -> Prim (l, p, c, [annot])
  | _ -> raise (Failure "annotate")

let seq s : unit michelson = Seq ((), s)


let get_loc : 'l michelson -> 'l = function
  | Prim (l, _, _, _) -> l
  | Seq (l, _) -> l
  | Int (l, _) -> l
  | String (l, _) -> l
  | Bytes (l, _) -> l

let int n : unit michelson = Int ((), n)
let string s : unit michelson = String ((), s)
let bytes s : unit michelson = Bytes ((), s)

let contract parameter storage code views =
  let views = List.map
    ~f:(fun (name, t_arg, t_ret, code) -> 
      prim ~children:[string name ; t_arg ; t_ret ; code] "view"
    )
    views
  in
  seq (
    [ prim ~children:[parameter] "parameter" ;
      prim ~children:[storage] "storage" ;
      prim ~children:[code] "code" ;
    ] @ views
  )

let t_unit = prim "unit"
let t_string = prim "string"
let t_bytes = prim "bytes"
let t_pair a b = prim ~children:[a;b] "pair"
let t_lambda a b = prim ~children:[a;b] "lambda"
let t_or a b = prim ~children:[a;b] "or"

let d_unit = prim "Unit"
let d_pair a b = prim ~children:[a;b] "Pair"

let i_dup = prim "DUP"
let i_car = prim "CAR"
let i_cdr = prim "CDR"
let i_pair = prim "PAIR"
let i_swap = prim "SWAP"
let i_piar = seq [ i_swap ; i_pair ]
let i_push ty code = prim ~children:[ty;code] "PUSH"
let i_push_unit = i_push t_unit d_unit
let i_push_string str = i_push t_string (string str)

let i_apply = prim "APPLY"

let i_comment s = seq [ i_push_string s ; prim "DROP" ]

let i_none ty = prim ~children:[ty] "NONE"
let i_nil ty = prim ~children:[ty] "NIL"
let i_empty_set ty = prim ~children:[ty] "EMPTY_SET"
let i_iter body = prim ~children:[body] "ITER"
let i_map body = prim ~children:[body] "MAP"
let i_some = prim "SOME"
let i_lambda arg ret body = prim ~children:[arg;ret;body] "LAMBDA"
let i_empty_map src dst = prim ~children:[src;dst] "EMPTY_MAP"
let i_empty_big_map src dst = prim ~children:[src;dst] "EMPTY_BIG_MAP"
let i_drop = prim "DROP"
let i_dropn n = prim "DROP" ~children:[int (Z.of_int n)]
let i_exec = prim "EXEC"

let i_if a b = prim ~children:[seq [a] ; seq[b]] "IF"
let i_if_none a b = prim ~children:[seq [a] ; seq[b]] "IF_NONE"
let i_if_cons a b = prim ~children:[seq [a] ; seq[b]] "IF_CONS"
let i_if_left a b = prim ~children:[seq [a] ; seq[b]] "IF_LEFT"
let i_failwith = prim "FAILWITH"
let i_assert_some = i_if_none (seq [i_push_string "ASSERT_SOME" ; i_failwith]) (seq [])
let i_assert_some_msg msg = i_if_none (seq [msg ; i_failwith]) (seq [])

let dip code = prim ~children:[seq [code]] "DIP"
let dipn n code = prim ~children:[Int (() , Z.of_int n) ; seq [code]] "DIP"
let i_dig n = prim ~children:[Int (() , Z.of_int n)] "DIG"
let i_dug n = prim ~children:[Int (() , Z.of_int n)] "DUG"
let i_unpair = seq [i_dup ; i_car ; dip i_cdr]
let i_unpiar = seq [i_dup ; i_cdr ; dip i_car]

let i_loop_left body = prim ~children:[seq [body]] "LOOP_LEFT"

let rec strip_annots = function
  | Seq(l, s) -> Seq(l, List.map ~f:strip_annots s)
  | Prim (l, p, lst, _) -> Prim (l, p, List.map ~f:strip_annots lst, [])
  | x -> x

let pp ppf michelson =
  let open Micheline_printer in
  let canonical = strip_locations michelson in
  let node = printable (fun s -> s) canonical in
  print_expr ppf node

let get_json michelson =
  let open Micheline_printer in
  let canonical = strip_locations michelson in
  let node = printable (fun s -> s) canonical in
  Data_encoding.(
      Json.construct
        (Micheline.erased_encoding ~variant:"???" {comment = None} Data_encoding.string)
        node
    )

let pp_json ppf michelson =
  let json = get_json michelson in
  Format.fprintf ppf "%a" Data_encoding.Json.pp json
