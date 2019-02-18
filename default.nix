let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./shell.nix {}
