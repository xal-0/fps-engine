{ pkgs ? import <nixpkgs> { } }:

let
  hpkgs = pkgs.haskellPackages.extend (self: super:
    {
      GPipe-Core = pkgs.haskell.lib.doJailbreak super.GPipe-Core;
      GPipe-GLFW4 = pkgs.haskell.lib.dontCheck super.GPipe-GLFW4;
    }
    // (pkgs.haskellPackages.packageSourceOverrides { fps-engine = ./.; }) self
    super);

in hpkgs.shellFor {
  packages = p: [ p.fps-engine ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    renderdoc
    ghcid
    haskell-language-server
    haskellPackages.threadscope
    haskellPackages.hp2pretty
    haskellPackages.profiteur
  ];
}
