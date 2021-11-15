
type 'a coq_sig = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| Coq_existT of 'a * 'p


