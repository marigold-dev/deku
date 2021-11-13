type storage = {
   tokenPool : nat ;
   cashPool : nat ;
   lqtTotal : nat ;
   pendingPoolUpdates : nat ;
   tokenAddress : address ;
   tokenId : nat ;
   cashAddress : address ;
   lqtAddress :  address ;
}

let initial_storage = 
  { tokenPool = 103n ;
    cashPool = 204n ;
    lqtTotal = 10n ;
    pendingPoolUpdates = 0n ;
    tokenAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) ;
    tokenId = 0n ;
    cashAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) ;
    lqtAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) ;
  }

let test =
  assert (initial_storage = initial_storage)

// Nested values::
let hello: string = "Hello"
let addr1 = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let addr2 = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address)
let ts1 = ("2000-01-01t10:10:10Z" : timestamp)
let ts2 = ("2000-01-02t10:10:10Z" : timestamp)
// list
let lst: int list = [1;2;3]
// record
type r = {
    i: int;
    s: string;
    n: nat;
    t: tez;
    a: address;
    ts: timestamp;
}
let r = {
    i= 1;
    s= "hello";
    n= 2n;
    t= 1tez;
    a= addr1;
    ts= ts1;
}
let r2 = {
    i= 2;
    s= "hell0";
    n= 3n;
    t= 1tez;
    a= addr1;
    ts= ts1;
}
// set
let s: int set = Set.literal [1; 2; 3; 4]
let s2: int set = Set.literal [3; 4; 5; 6]
// map
let m: (int, string) map = Map.literal [
    (1, "hello");
    (2, "world");
]
let m2: (int, string) map = Map.literal [
    (1, "hell0");
    (2, "w0rld");
    (3, "foo");
]
// big_map
let bm: (int, string) big_map = Big_map.literal [
    (1, "hello");
    (2, "world");
]
let bm2: (int, string) big_map = Big_map.literal [
    (1, "hell0");
    (2, "w0rld");
    (3, "foo");
]
// contructor
// constructor - list
// constructor - record
// constructor - set
// constructor - map
// constructor - constructor
type simpl =
      Simpl
    | Str of string
type c =
      Lst of int list
    | Rcd of r
    | S of int set
    | M of (int, string) map
    | Nest of simpl
let c1: simpl = Simpl
let c2: simpl = Str "Hello"
let c3: c = Lst lst
let c4: c = Rcd r
let c5: c = S s
let c6: c = M m
let c7: c = Nest c1
let c8: c = Nest c2
// list - records
let lr: r list = [r;r;r]
let lr2: r list = [r2;r;r2]
// list - set
let ls: int set list = [s;s;s]
let ls2: int set list =[s2;s;s2]
// list - contructor
let lcs: c list = [c3;c4;c5;c6;c7;c8]
let lcs2: c list = [c8;c4;c5;c6;c7;c3]
// list - map
let lm: (int, string) map list = [m;m;m]
let lm2: (int, string) map list = [m2;m2;m]
// list - big_map
let lbm: (int, string) big_map list = [bm;bm;bm]
let lbm2: (int, string) big_map list = [bm2;bm2;bm]
// list - list
let ll: int list list = [lst;lst;lst]
let ll2: int list list = [lst;[3;2;1];lst]
// record - list + record + set + constructor + map
type nest_r = {
    l: int list;
    r: r;
    s: int set;
    c: c;
    m: (int, string) map
}
let nest_r = {
    l= lst;
    r= r;
    s= s;
    c= c8;
    m= m;
}
let nest_r2 = {
  l= [3; 2; 1];
  r= r;
  s= s;
  c= c7;
  m= m;
}
// set - list
let sl: int list set = Set.literal [lst; lst; lst]
let sl2: int list set = Set.literal [[3; 2; 1]; lst; lst]
// set - records
let sr: r set = Set.literal [r; r; r]
let sr2: r set = Set.literal [r2; r; r]
// set - contructor
let sc: c set = Set.literal [c8; c8; c8]
let sc2: c set = Set.literal [c8; c7; c8]
// set - map
let sm: (int, string) map set = Set.literal [m; m; m]
let sm2: (int, string) map set = Set.literal [m; m2; m]
// set - set
let ss: int set set = Set.literal [s; s; s]
let ss2: int set set = Set.literal [s; s; s2]
// map - list
let ml: (int, int list) map = Map.literal [(1, lst)]
let ml2: (int, int list) map = Map.literal [(1, [3;2;1])]
// map - map
let mm: (int, (int, string) map) map = Map.literal [(1, m)]
let mm2: (int, (int, string) map) map = Map.literal [(2, m)]
// big_map - map
let bmm: (int, (int, string) map) big_map = Big_map.literal [(1, m)]
let bmm2: (int, (int, string) map) big_map = Big_map.literal [(2, m)]
// map - big_map
let mbm: (int, (int, string) big_map) map = Map.literal [(1, bm)]
let mbm2: (int, (int, string) big_map) map = Map.literal [(2, bm)]
// map - set
let ms: (int, int set) map = Map.literal [(1, s)]
let ms2: (int, int set) map = Map.literal [(1, s2)]
// map - constructor
let mc: (int, c) map = Map.literal [(1, c8)]
let mc2: (int, c) map = Map.literal [(1, c7)]
// map - record
let mr: (int, r) map = Map.literal [(1, r)]
let mr2: (int, r) map = Map.literal [(1, r2)]

let test_equal =
  let _ = assert (hello = hello) in
  let _ = assert (addr1 = addr1) in
  let _ = assert (ts1 = ts1) in
  let _ = assert (lst = lst) in
  let _ = assert (r = r) in
  let _ = assert (s = s) in
  let _ = assert (m = m) in
  let _ = assert (bm = bm) in
  let _ = assert (c1 = c1) in
  let _ = assert (c2 = c2) in
  let _ = assert (c3 = c3) in
  let _ = assert (c4 = c4) in
  let _ = assert (c5 = c5) in
  let _ = assert (c6 = c6) in
  let _ = assert (c7 = c7) in
  let _ = assert (c8 = c8) in
  let _ = assert (lr = lr) in
  let _ = assert (ls = ls) in
  let _ = assert (lcs = lcs) in
  let _ = assert (lm = lm) in
  let _ = assert (lbm = lbm) in
  let _ = assert (ll = ll) in
  let _ = assert (nest_r = nest_r) in
  let _ = assert (sl = sl) in
  let _ = assert (sr = sr) in
  let _ = assert (sc = sc) in
  let _ = assert (sm = sm) in
  let _ = assert (ss = ss) in
  let _ = assert (ml = ml) in
  let _ = assert (mm = mm) in
  let _ = assert (bmm = bmm) in
  let _ = assert (mbm = mbm) in
  let _ = assert (ms = ms) in
  let _ = assert (mc = mc) in
  let _ = assert (mr = mr) in
  ()

let test_not_equal =
  let _ = assert (hello <> "Bye") in
  let _ = assert (addr1 <> addr2) in
  let _ = assert (ts1 <> ts2) in
  let _ = assert (lst <> [3;2;1]) in
  let _ = assert (r <> r2) in
  let _ = assert (s <> s2) in
  let _ = assert (m <> m2) in
  let _ = assert (bm <> bm2) in
  let _ = assert (c1 <> c2) in
  let _ = assert (c2 <> c1) in
  let _ = assert (c3 <> c4) in
  let _ = assert (c4 <> c5) in
  let _ = assert (c5 <> c6) in
  let _ = assert (c6 <> c7) in
  let _ = assert (c7 <> c8) in
  let _ = assert (c8 <> c3) in
  let _ = assert (lr <> lr2) in
  let _ = assert (ls <> ls2) in
  let _ = assert (lcs <> lcs2) in
  let _ = assert (lm <> lm2) in
  let _ = assert (lbm <> lbm2) in
  let _ = assert (ll <> ll2) in
  let _ = assert (nest_r <> nest_r2) in
  let _ = assert (sl <> sl2) in
  let _ = assert (sr <> sr2) in
  let _ = assert (sc <> sc2) in
  let _ = assert (sm <> sm2) in
  let _ = assert (ss <> ss2) in
  let _ = assert (ml <> ml2) in
  let _ = assert (mm <> mm2) in
  let _ = assert (mbm <> mbm2) in
  let _ = assert (bmm <> bmm2) in
  let _ = assert (ms <> ms2) in
  let _ = assert (mc <> mc2) in
  let _ = assert (mr <> mr2) in
  ()
