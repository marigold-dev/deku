type comb_two = [@layout:tree] {
  [@annot:anfoo]
  foo : int ;
  [@annot:anbar]
  bar : string ;
}

type comb_three = [@layout:tree] {
  [@annot:ana]
  a : int ;
  [@annot:anb]
  b : string ;
  [@annot:anc] 
  c : nat ;
}

type comb_five = [@layout:tree] {
  [@annot:an_One]
  one : int ;
  [@annot:an_Two]
  two : string ;
  [@annot:an_Three]
  three : bool;
  [@annot:an_Four]
  four : nat ;
  [@annot:an_Five]
  five : int ;
}

type parameter = unit
type op_list = operation list

let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = store.foo in
  let oo = { store with foo = o } in
  ([] : operation list), oo

let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  ([] : operation list), { a = 1 ; b = "" ; c = 1n }

let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  ([] : operation list), store