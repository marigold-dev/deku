{ pkgs, esy ? pkgs.nodePackages.esy }:

let 
  rely = pkgs.ocaml-ng.ocamlPackages_5_00.reason-native.rely.overrideAttrs (_: {
    postPatch = ''
      substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
    '';
  });
in

pkgs.mkShell {
  shellHook  = ''
    mv package.json p.json
    npm install webpack webpack-cli @taquito/taquito @taquito/signer @taquito/rpc
    mv p.json package.json
    export PATH=$PATH:./node_modules/.bin
  '';
  buildInputs = (with pkgs; [
    # Make developer life easier
    ## General tooling
    docker
    docker-compose
    nodejs

    # Nix files formatter
    nixfmt
  ]) ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
      # OCaml developer tooling
    ocaml
    findlib
    dune_2
    # esy
    ocaml-lsp
    ocamlformat-rpc
  ]);

  propagatedBuildInputs = with pkgs.ocaml-ng.ocamlPackages_5_00; [
    ppx_deriving
    ppx_deriving_yojson
    lwt
    dream
    mirage-crypto
    mirage-crypto-pk
    mirage-crypto-rng
    mirage-crypto-ec
    piaf
    mrmime
    hex
    tezos-micheline
    digestif
    cmdliner
    ppx_blob
    secp256k1-internal
    bigstring
    domainslib
    utop
    reason
    rely
  ];
}