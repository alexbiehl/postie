{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.lib.compose.doJailbreak
  (pkgs.haskellPackages.callCabal2nix "postie" ./. {})
