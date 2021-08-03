{ pkgsc ? import <nixpkgs>, ... }:

let
  overlay = self: super: {
    haskellPackages =
      super.haskellPackages.override {
        overrides = hself: hsuper: with self.haskell.lib; {
          GPipe-Core = self.haskell.lib.doJailbreak hsuper.GPipe-Core;
          GPipe-GLFW4 = pkgs.haskell.lib.dontCheck hsuper.GPipe-GLFW4;
        };
      };
  };
  pkgs = pkgsc {
    overlays = [ overlay ];
  };

in

pkgs.haskellPackages.callCabal2nix "physics" ./. { }
