{
  description = "Simply Typed Lambda Calculus";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils/master";
  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      with (import nixpkgs { inherit system; }); {
        packages = flattenTree {
          rutwe =
            with haskell.lib;
            with haskellPackages;
            overrideCabal (callCabal2nix "rutwe" ./. {}) (_: {});
        };
      });
}
