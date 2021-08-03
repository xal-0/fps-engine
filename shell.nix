{ pkgsc ? import <nixpkgs> }:

let pkgs = pkgsc { };

in

pkgs.mkShell {
  packages = with pkgs; [
    renderdoc
    cabal-install
    cabal2nix
  ];
  inputsFrom = [ (import ./default.nix { inherit pkgsc; }).env ];
}
