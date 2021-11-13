# Nix expressions for building LIGO

Nix is a declarative package manager. Get it here: https://nixos.org/nix

These expressions are used on CI to reproducibly build the LIGO compiler, as well as WebIDE and https://ligolang.org .

If you wish to build it yourself, do `nix build -f. $thing`, where `$thing` is

- `ligo-editor`: WebIDE, it can be started with `result/bin/ligo-editor`
- `ligo-website`: the website, website root is `result`
- `ligo-editor-docker`: a docker image with webide
- `ligo-deb`: debian package with static binaries
- `ligo-changelog`: changelogs

The output of `nix build` can be found in `result` directory.

To see the logs while building, use the [secret
`-L`|`--print-build-logs`
option](https://github.com/NixOS/nix/issues/1904#issuecomment-706518776)
to `nix build`.

## Quick maintenance guide

- `opam-repository` is pinned. To update it when required, run `niv update` (you can get niv with `nix shell 'nixpkgs#niv'`)
- `ocaml` version is pinned in `ocaml-overlay.nix`. If you want to update it, go there and change the version.
- If something fails, `nix repl pkgs.nix` can be very useful to investigate it.

## Known caveats

- This is not a [nix flake](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7). This will never be a flake if we want to keep this low-maintenance, because of the way `opam` sources are defined. Sometimes, the checksum is omitted there, so we have to use `fetchTarball` without the checksum, which won't work in restricted mode (which is required for flakes). The only solution would be to generate nix expressions for opam-repository separately, but it means a manual step in the process (and it's also impossible to make this work as a flake).
- For the same reason as above, evaluation can take a while because we need to download all the sources every `tarball-ttl` seconds. This can be mitigated by setting `tarball-ttl` to a high value.
