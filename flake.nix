{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };
  nixConfig.sandbox = "relaxed";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        treeSurgeon = (
          pkgs.haskellPackages.callCabal2nix "tree-surgeon" ./src/. {}
        ).overrideAttrs (prev: { buildInputs = with pkgs; prev.buildInputs ++ [ alex happy ]; });
        overlay = (final: prev: {
          inherit treeSurgeon;
        });
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        createDocumentationLocally = pkgs.writeShellApplication {
          name = "create_documentation_locally";
          runtimeInputs = with pkgs; [ asciidoctor plantuml ];
          text = ''
            if [[ "$(basename "$(pwd)")" != "tree-surgeon" ]]; then
                echo "Please run from root folder"
                exit 1
            else
                rm -rf docs_test
                mkdir docs_test
                asciidoctor -a docinfo=shared -D docs_test '**/*.adoc' -v
            fi
          '';
        };
        runExecutableTests = pkgs.writeShellApplication {
          name = "run_executable_tests";
          runtimeInputs = with pkgs; [ babashka ];
          text = ''
            PWD=$(pwd)
            cd src
            bb test/executable-tests.clj
            cd "$PWD"
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
        devShell = pkgs.haskellPackages.shellFor {
          name = "tree-surgeon";
          packages = p: [ treeSurgeon ];
          withHoogle = true;
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            alex_3_4_0_0
            asciidoctor
            babashka
            cabal-install
            createDocumentationLocally
            happy
            plantuml
          ];
          shellHook = "command -v fish &> /dev/null && fish";
        };
        packages = {
          tree-surgeon = treeSurgeon;
          default = treeSurgeon;
        };
      }
    );
}
