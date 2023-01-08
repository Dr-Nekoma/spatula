{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { flake-utils, nixpkgs, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {};
        overlays = [];
        pkgs = import nixpkgs { inherit config overlays system; };
        haskellPackages = pkgs.haskellPackages;
        packageName = "spatula";
      in rec {
        packages.${packageName} =
          # Ignore test cases
          pkgs.haskell.lib.dontCheck (
            haskellPackages.callCabal2nix packageName self rec {
              # Dependency overrides go here
            }
          );

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          inputsFrom = builtins.attrValues self.packages.${system};
          withHoogle = true;

          packages = p: [
          ];

          buildInputs = with haskellPackages; [
            cabal-install
            stack

            # Extra dev tools
            ghcid
            haskell-language-server
            hlint
            ormolu
          ];
        };
      }
    );
}
