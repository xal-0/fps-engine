{ pkgs ? import <nixpkgs> { } }:

(import ./. { inherit pkgs; }).shellFor {
  packages = p: [ p.halflife ];
  withHoogle = true;
  buildInputs = with pkgs; [ cabal-install renderdoc haskellPackages.hoogle ];
}
