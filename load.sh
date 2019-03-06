#!/bin/sh

nix-shell -p 'haskellPackages.ghcWithPackages (pkgs: [ (pkgs.callPackage ./jooj.nix {}) ])' --run ghci
