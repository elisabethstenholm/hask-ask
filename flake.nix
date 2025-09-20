{
  description = "Hask ask – A small auction platform in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        packageName = "hask-ask";

        # Build the Haskell package from your cabal project
        package = pkgs.haskellPackages.callCabal2nix packageName ./. { };
      in
      {
        packages.${packageName} = package;
        packages.default = package;

        apps.${packageName} = {
          type = "app";
          program = "${package}/bin/${packageName}";
        };
        apps.default = self.apps.${system}.${packageName};

        # Dev shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            zlib
          ];
        };
      }
    );
}