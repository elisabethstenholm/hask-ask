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

        # Create the package with explicit dependencies
        package = pkgs.haskellPackages.callCabal2nix packageName ./. { } ;
        
      in {
        packages.${packageName} = package;
        packages.default = package;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell development tools
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ];
        };
      }
    );
}