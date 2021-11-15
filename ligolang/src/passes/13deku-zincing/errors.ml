(* 
   This type is actually bigger than it could be, 
   since not all spilling errors are possible, but 
   reducing it would require bigger changes I don't 
   feel like doing right now
*)
type zincing_error = Spilling.Errors.spilling_error
