{
  nix-filter,
  lib,
  rustPlatform,
  m4,
  gmp,
}:

rustPlatform.buildRustPackage rec {
  pname = "vm_library";
  version = "1.0.0";

  src = with nix-filter.lib; filter {
    root = ../.;
    include = [
      "crates"
      "Cargo.lock"
      "Cargo.toml"
    ];
  };

  cargoSha256 = "sha256-9UsWv5aKzqskfYXL/SclKCKtw/1XG2C7fI0oCGJR5sA=";

  nativeBuildInputs = [
    m4
  ];

  buildInputs = [
    gmp
  ];

  doCheck = false;
}
