{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkg = pkgs.haskellPackages.developPackage { root = ./.; };
      in
      {
        packages = rec {
          hattier = pkg;
          default = hattier;
        };
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = _: [ pkg ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            ghc
            haskellPackages.cabal-fmt
            haskellPackages.hindent
            haskell-language-server
            hlint
            zlib
          ];
        };
      }
    );
}
