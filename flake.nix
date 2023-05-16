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
        createDocumentationLocally = pkgs.writeShellApplication {
          name = "create_documentation_locally";
          runtimeInputs = with pkgs; [ asciidoctor ];
          text = ''
            if [[ "$(basename "$(pwd)")" != "tree-surgeon" ]]; then
                echo "Please run from root folder"
                exit 1
            else
                rm -rf docs_test
                mkdir docs_test
                asciidoctor -a docinfo=shared -D docs_test '**/*.adoc'
            fi
          '';
        };
      in
      {
        apps = {
          create_documentation_locally = {
            type = "app";
            program = "${createDocumentationLocally}/bin/create_documentation_locally";
          };
        };
        packages = { tree-surgeon = treeSurgeon; };
        defaultPackage = treeSurgeon;
        devShell = pkgs.haskellPackages.shellFor {
            name = "tree-surgeon";
            packages = p: [ treeSurgeon ];
            withHoogle = true;
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              cabal-install
              happy
              alex
              asciidoctor
              createDocumentationLocally
            ];
          shellHook = "command -v fish &> /dev/null && fish";
          };
       }
    );
}
