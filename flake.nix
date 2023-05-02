{
  description = "Utility to convert dhall records and types to Python";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        treeSurgeon = pkgs.haskellPackages.callCabal2nix "tree-surgeon" ./. {};
        overlay = (final: prev: {
          inherit treeSurgeon;
        });
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages = { tree-surgeon = treeSurgeon; };
        defaultPackage = treeSurgeon;
        devShell = pkgs.haskellPackages.shellFor {
            name = "tree-surgeon";
            packages = p: [ treeSurgeon ];
            withHoogle = true;
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              # haskell-language-server
              # haskell.compiler.ghc924
              # ghcid
              cabal-install
              happy
              alex
            ];
          shellHook = "command -v fish &> /dev/null && fish";
          };
       }
    );
}
