{
  description = "An OCaml development environment";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      with pkgs;
      {
        devShell = mkShell rec {
          nativeBuildInputs = [ 
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
          ];
          buildInputs = [
            ocaml
            # opam
            ocamlPackages.findlib
            ocamlPackages.ppx_inline_test
            dune_3
          ];
          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      }
    );
}
