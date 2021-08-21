{ pkgs ? import <nixpkgs> { } }:

(import ./. { inherit pkgs; }).shellFor {
  packages = p: [ p.halflife ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    renderdoc
    ghcid
    haskellPackages.hoogle
    haskellPackages.threadscope
    haskellPackages.hp2pretty
    haskellPackages.profiteur
  ];
}
