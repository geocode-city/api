let
    nixpkgs = import ./nix/packages.nix {};
    lib = import ./nix/release.nix;

in

    nixpkgs.haskellPackages.shellFor {
        packages = p: builtins.attrValues lib;
        buildInputs = [
            nixpkgs.haskellPackages.cabal-install
            nixpkgs.haskellPackages.haskell-language-server
            nixpkgs.haskellPackages.cabal2nix
            nixpkgs.haskellPackages.hackage-db
        ];
    }
