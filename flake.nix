{
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
        runExecutableTests = pkgs.writeShellApplication {
          name = "run_executable_tests";
          runtimeInputs = with pkgs; [ babashka ];
          text = ''
            bb test/executable-tests.clj
          '';
        };
      in
      {
        apps = {
          create_documentation_locally = {
            type = "app";
            program = "${createDocumentationLocally}/bin/create_documentation_locally";
          };
          run_executable_tests = {
            type = "app";
            program = "${runExecutableTests}/bin/run_executable_tests";
          };
        };
        packages = {
          tree-surgeon = treeSurgeon;
          default = treeSurgeon;
        };
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
            babashka
          ];
          shellHook = "command -v fish &> /dev/null && fish";
        };
      }
    );
}
