This is a devcontainer designed for doing OCaml/Reason development on Tezos using Esy.

To create a new Reason project, a good template is available [here](https://github.com/esy-ocaml/hello-reason). To get editor integration, you'll have to follow the instructions in step #3. 

Included in this devcontainer:
==============================

1) Ligo

2) esy

3) ocaml-platform

   Make sure you have `"devDependencies": { ..., "@opam/ocaml-lsp-server": "*"}` in your package.json, then run `esy`. (You might need to refresh the window too.)

4) A Tezos node. Use `mainnet.sh start` to start it, then use `mainnet.sh client` to interact with the client

   To actually use it, you're probably going to want to bootstrap it which takes forever to do from scratch, so make your life easier and get a snapshot. Download a mainnet snapshot from [here](https://snapshots-tezos.giganode.io/) (rolling or full, doesn't matter) then run:

   ```
   mainnet.sh stop
   # Stop the node if it's running

   mainnet.sh clear
   # Clear anything it's already downloaded

   mainnet.sh snapshot import /full/path/to/snapshot && mainnet.sh client start && mainnet.sh client bootstrapped 
   # Import the snapshot and then finish bootstrapping
   # This will take a moment, so get a coffee
   ```

5) Some other extensions I find useful, like Git Graph and Indent Rainbow.

