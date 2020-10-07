{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  packages = _: [ (import ./default.nix { inherit pkgs; }) ];
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    hlint
    ormolu
  ];
}
