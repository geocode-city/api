let
  pkgs = import ./packages.nix {};
in
  { geocode-city = pkgs.haskellPackages.geocode-city-api; }
