type comb_two = [@layout:tree]
  | [@annot:anbfoo] Foo of int
  | [@annot:anabar] Bar of string

type comb_three = [@layout:tree]
  | [@annot:ana] A of int
  | [@annot:anb] B of string
  | [@annot:anc] C of nat

type comb_five = [@layout:tree]
  | [@annot:an_One] One of int
  | [@annot:an_Two] Two of string
  | [@annot:an_Three] Three of bool
  | [@annot:an_Four] Four of nat
  | [@annot:an_Five] Five of int

type parameter = unit
type op_list = operation list


let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = match store with
    | Foo i -> Bar "foo"
    | Bar j -> Foo 1
  in
 ([] : operation list), o
 
let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  let o = (C 1n) in
  ([] : operation list), o
  
let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  let o = match store with
    | One a -> Five (1)
    | Two a -> Four (2n)
    | Three a -> Three (true)
    | Four a -> Two ("lol")
    | Five a -> One 1
  in
  ([] : operation list), o