(* open Deku_crypto
   open Deku_concepts
   open Deku_consensus

   let id = Secret.Ed25519 (Ed25519.Secret.generate ())
   let validators =
     (Key.of_secret id |> Key_hash.of_key )
     List.init 3 (fun _ ->
         Secret.Ed25519 (Ed25519.Secret.generate ())
         |> Key.of_secret |> Key_hash.of_key)


   let initial_block =
     let identity = Identity.make id in
     let level = Level.zero in
     let previous = Block_hash.hash "tuturu" in
     let operations = [] in
     Block.produce ~identity ~level ~previous ~operations

   let initial_consensus =
     Consensus.make ((id validators) *)
