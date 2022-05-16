open Cmdliner
open Crypto
open Node
open Helpers
open (
  struct
    include Server
  end :
    sig end)

let get_current_block_level () = Lwt.return 2L

let transfers_spam_tickets ~ticketer = ()

let load_test_transactions ticketer = ()

let load_test_transactions ticketer = load_test_transactions ticketer
