(* type _ syscall =
     | FAILWITH         : int -> bytes syscall
     | EXAMPLE          : int -> (int * string) syscall
     | SENDER           : int -> (Address.t -> unit) syscall
     | SOURCE           : int -> (Address.t -> unit) syscall
     | SELF             : int -> (Address.t -> unit) syscall
     | OWN_TICKET       :
         int
         -> (Ticket_handle.t * (Ticket_handle.t -> unit)) option syscall
     | READ_TICKET      :
         int
         -> (Ticket_handle.t
            * (Ticket_handle.t -> Amount.t -> Ticket_handle.t -> unit))
            option
            syscall
     | JOIN_TICKETS     :
         int
         -> (Ticket_handle.t * Ticket_handle.t * (Ticket_handle.t -> unit)) option
            syscall
     | SPLIT_TICKET     :
         int
         -> (Ticket_id.t * (Ticket_handle.t -> Ticket_handle.t -> unit)) option
            syscall
     | GET_CONTRACT_OPT :
         int
         -> (Address.t * (Address.t option -> unit)) option syscall
     | TRANSACTION      :
         int
         -> (bytes * Ticket_handle.t -> Address.t * (unit -> unit)) option syscall

   (* THIS IS AN OVERKILL *)
   let load : type a. a syscall -> a = function
     | FAILWITH _ -> "tyest" |> String.to_bytes
     | SENDER _ -> fun _ -> ()
     | _ -> failwith "est"

   module type CTX = sig
     val failwith : bytes -> unit
     val sender : (Address.t -> unit) -> unit
   end

   let syscall (type a) mem (module S : CTX) =
     let module T = S in
     fun x ->
       match Array.get mem x with
       | 0 -> load (FAILWITH 0) |> T.failwith
       | 1 -> load (SENDER 1) |> T.sender
       | _ -> failwith "test"

   module S : CTX = struct
     let sender = Address.of_string "test" |> Option.get
     let failwith _ = ()
     let sender f = f sender
   end
   let res = syscall [|0|] (module S) 1 *)
