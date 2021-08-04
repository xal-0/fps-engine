{ pkgs ? import <nixpkgs> { } }:

let
  h0 = pkgs.haskellPackages.extend (pkgs.haskellPackages.packageSourceOverrides {
    GPipe-Core = ./GPipe-Core;
  });

  h1 = h0.extend (
    self: super: {
      GPipe-GLFW4 = pkgs.haskell.lib.dontCheck super.GPipe-GLFW4;
    }
  );

  h2 = h1.extend (h1.packageSourceOverrides {
    halflife = ./.;
  });

in

h2
